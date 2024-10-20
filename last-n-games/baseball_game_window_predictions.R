library(baseballr)
library(tidyverse)
library(here)

font <- 'Titillium Web'
sysfonts::font_add_google(font, font)
showtext::showtext_auto()


#===================================================================================================================
# Get Data ----------------------------------------------------------------

# # commenting this out, but if uncomment if need to re-run
# schedule <- c(1990:2009) |> purrr::map_df(baseballr::mlb_schedule)
# schedule_new <- c(2010:2023) |> purrr::map_df(baseballr::mlb_schedule)
# schedule <- bind_rows(
#   schedule,
#   schedule_new
# )
# 
# saveRDS(schedule, here("last-n-games", "mlb_schedule.rds"))
# 
# playoffs_older <- c(1990, 1991, 1992, 1993, 1995, 1996, 1997, 1998, 1999) |> purrr::map_df(baseballr::mlb_schedule_postseason_series)
# playoffs_raw <- c(2000:2023) |> purrr::map_df(baseballr::mlb_schedule_postseason_series)
# 
# playoffs_raw <- playoffs_older |> bind_rows(playoffs_raw)
# 
# saveRDS(playoffs_raw, here("last-n-games", "mlb_playoffs_raw.rds"))




#===================================================================================================================
# Analysis ----------------------------------------------------------------

schedule <- readRDS(here("last-n-games", "mlb_schedule.rds"))
playoffs_raw <- readRDS(here("last-n-games", "mlb_playoffs_raw.rds"))

schedule_trim <- schedule |> 
  select(date, resume_game_date, resumed_from_date, season, total_games, game_pk, game_type, game_number, day_night, description, scheduled_innings, games_in_series, series_game_number, series_description,
         status_detailed_state, teams_away_team_name, teams_away_score, teams_home_team_name, teams_home_score, reschedule_game_date) |> 
  filter(series_description %in% c("Regular Season", "Wild Card Game", "Division Series", "League Championship Series", "World Series")) |> 
  mutate(is_playoff = case_when(
    series_description %in% c("Wild Card Game", "Division Series", "League Championship Series", "World Series") ~ "playoff",
    TRUE ~ "regular"
  )) |> 
  filter(status_detailed_state %in% c("Completed Early", "Final"))


df_long <- schedule_trim |> 
  select(game_pk, date, resume_game_date, resumed_from_date, season, is_playoff, description, series_description, status_detailed_state, team=teams_home_team_name, score=teams_home_score, opponent=teams_away_team_name, score_against=teams_away_score) |> 
  mutate(home_away="home") |> 
  bind_rows(
    schedule_trim |> 
      select(game_pk, date, resume_game_date, resumed_from_date, season, is_playoff, description, series_description, status_detailed_state, team=teams_away_team_name, score=teams_away_score, opponent=teams_home_team_name, score_against=teams_home_score) |> 
      mutate(home_away="away")
  ) |> 
  mutate(finish_date = ifelse(is.na(resume_game_date), date, resume_game_date)) |> 
  arrange(team, season, game_pk, desc(finish_date)) |> 
  distinct(team, game_pk, .keep_all = T) |> 
  mutate(result = case_when(
    score > score_against ~ 1,
    score < score_against ~ 0,
    TRUE ~ 0.5
  )) |> 
  group_by(season, team) |> 
  mutate(season_game_num = row_number(),
         games_played = max(season_game_num)) |> ungroup() |> 
  filter(season!= 2020) |> # remove the covid season
  filter(!season %in% c(1994, 1995)) # remove the interrupted seasons - lockout



# now only keep regular season games
regular_season <- df_long |> 
  # mutate(is_tb = ifelse(str_detect(tolower(description), "tiebreak"), "y", "n")) |> 
  filter(is_playoff != "playoff")

# Tie Break games are labelled regular season, however we will treat them as playoff games
rem_idx <- grep("tiebreak", tolower(regular_season$description))

regular_season <- regular_season[-rem_idx, ]

regular_season <- regular_season |> 
  group_by(season, team) |> 
  mutate(season_game_num = row_number(),
         games_played = max(season_game_num)) |> ungroup() |> 
  mutate(team_season = paste(season, team, sep = "-"))


#===================================================================================================================
# Fit win % per games remaining -------------------------------------------

win_pct_df <- data.frame()

games <- seq(20, 162, 1)

for(i in games) {
  df <- regular_season |> 
    filter(season_game_num > (games_played - i)) |> 
    group_by(season, team) |> 
    summarise(n_wins = sum(result),
              win_pct = mean(result), .groups = "drop") |> 
    mutate(games_remaining = i)
  
  win_pct_df <- bind_rows(win_pct_df, df)
}


#===================================================================================================================
# Prep Playoff Data -------------------------------------------------------

playoffs <- playoffs_raw |> 
  filter(!season %in% c(1994, 1995, 2020)) |> # remove shortened seasons/no playoffs
  select(season, game_pk, official_date, series_description, series_id, total_games, series_game_number, status_detailed_state, 
         teams_home_team_name, teams_home_score, teams_away_team_name, teams_away_score) |> 
  filter(status_detailed_state == "Final")
  

playoff_matchups <- playoffs |> 
  arrange(season, series_id, official_date) |> 
  distinct(season, series_id, .keep_all = T) |> 
  select(season, series_id, total_games, teams_home_team_name, teams_away_team_name)



playoff_result <- playoffs |> 
  arrange(season, series_id, official_date) |> 
  distinct(season, series_id, game_pk, .keep_all = T) |> 
  group_by(season, series_id, team=teams_home_team_name) |> 
  summarise(n_games = n_distinct(game_pk),
            wins = sum(teams_home_score > teams_away_score), .groups = "drop") |> 
  bind_rows(
    playoffs |> 
      arrange(season, series_id, official_date) |> 
      distinct(season, series_id, game_pk, .keep_all = T) |> 
      group_by(season, series_id, team=teams_away_team_name) |> 
      summarise(n_games = n_distinct(game_pk),
                wins = sum(teams_away_score > teams_home_score), .groups = "drop")
  ) |> 
  group_by(season, series_id, team) |> 
  summarise(n_games = sum(n_games),
            wins=sum(wins), .groups = "drop")



playoff_matchups <- playoff_matchups |> 
  left_join(
    playoff_result,
    by = c("season", "series_id", "teams_home_team_name" = "team")
  ) |> 
  left_join(
    playoff_result |> select(-n_games),
    by = c("season", "series_id", "teams_away_team_name" = "team"),
    suffix = c("_home", "_away")
  ) |> 
  mutate(winner = ifelse(wins_home > wins_away, teams_home_team_name, teams_away_team_name))



#===================================================================================================================
# EDA ---------------------------------------------------------------------


analysis_df <- playoff_matchups |> 
  left_join(win_pct_df, by = c("season", "teams_home_team_name"="team")) |> 
  left_join(win_pct_df, by = c("season", "teams_away_team_name"="team", "games_remaining"),
            suffix = c("_home", "_away")) |> 
  distinct() |> 
  mutate(home_prob = win_pct_home / (win_pct_home + win_pct_away),
         away_prob = win_pct_away / (win_pct_home + win_pct_away)) |> 
  mutate(home_prob_home_edge = home_prob * 1.05,
         away_prob_home_edge = 1 - home_prob_home_edge) |> 
  mutate(home_win = ifelse(teams_home_team_name == winner, 1, 0))







p1 <- analysis_df |> 
  # filter(season >= 2010) |> 
  group_by(games_remaining) |>
  summarise(acc = mean(home_win == round(home_prob))) |> 
  # view() |> 
  ggplot(aes(x=games_remaining, y=acc)) +
  geom_point(size=3) +
  geom_smooth() +
  labs(x="Games Remaining") +
  scale_y_continuous(limits = c(0.40, 0.55), labels = scales::percent) +
  scale_x_reverse() +
  ggtitle("HOW MANY GAMES REMAINING IN THE REGULAR SEASON WOULD YOU NEED TO PREDICT MLB PLAYOFFS?",
          subtitle = "Cade Massey asked the Wharton moneyball team which N-games would you need left in the season to predict MLB playoff outcomes...\n\nThis method gets each playoff team's winning % after N games remaining, then calculates the playoff win probability based off this. For example, the Rangers had a win % of\n61.25% with 80 games remaining, while their 2023 WS opponent the Diamondbacks had a win % of 60% at the same point, giving the\nRangers 51.5% (61.25% / (61.25% + 60%)) win probability.\n\nThe results are very noisy. Peak accuracy (52.8%) comes at 33 games remaining when trying to predict the outcomes of any playoff series.\n\n1990-2023 seasons included (excluding 1994, 1994, 2020)") +
  labs(caption = "Data: MLB.com via {baseballr}\nData Visualisation: @jaseziv | dontblamethedata.com") +
  theme_minimal() +
  theme(text = element_text(family = font),
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "italic"),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        legend.position = "top", legend.text = element_text(size=13), panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(size = 15),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank())


p1



p2 <- analysis_df |> 
  mutate(playoff_round = case_when(
    str_detect(series_id, "F_|R_") ~ "1st",
    str_detect(series_id, "D_") ~ "2nd",
    str_detect(series_id, "L_") ~ "3rd",
    str_detect(series_id, "W_") ~ "4th"
  )) |> 
  group_by(playoff_round, games_remaining) |>
  summarise(acc = mean(home_win == round(home_prob))) |> 
  bind_rows(
    analysis_df |> 
      mutate(playoff_round = "overall") |> 
      group_by(playoff_round, games_remaining) |>
      summarise(acc = mean(home_win == round(home_prob)))
  ) |> 
  # view() |>
  ggplot(aes(x=games_remaining, y=acc)) +
  geom_point(aes(colour = playoff_round), size=1, alpha=0.3, position = "jitter") +
  geom_smooth(aes(colour = playoff_round), se=F) +
  labs(x="Games Remaining") +
  scale_y_continuous(limits = c(0.3, 0.65), labels = scales::percent) +
  scale_x_reverse() +
  scale_colour_manual(values = c("steelblue", "orange", "darkgreen", "darkred", "black"), name = "Playoff Round") +
  ggtitle("HOW MANY GAMES REMAINING IN THE REGULAR SEASON WOULD YOU NEED TO PREDICT EACH ROUND OF THE MLB PLAYOFFS?",
          subtitle = "The 'overall' group is all playoff matchups... Using this method has the strongest predictive power to predict the WS winner ('4th' round) by using the\nlast 30-40 games of the regular season, while the whole season is needed to predict the first (Wild Cards) and third (League Championship) rounds.\n\n1990-2023 seasons included (excluding 1994, 1994, 2020)") +
  labs(caption = "Data: MLB.com via {baseballr}\nData Visualisation: @jaseziv | dontblamethedata.com") +
  theme_minimal() +
  theme(text = element_text(family = font),
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "italic"),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        legend.position = "top", legend.text = element_text(size=13), panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(size = 15),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank())


p2


# test |> 
#   mutate(playoff_round = case_when(
#     str_detect(series_id, "F_|R_") ~ "1st",
#     str_detect(series_id, "D_") ~ "2nd",
#     str_detect(series_id, "L_") ~ "3rd",
#     str_detect(series_id, "W_") ~ "4th"
#   )) |> 
#   mutate(id = paste0(season, "-", series_id)) |> 
#   group_by(playoff_round) |>
#   summarise(n_games = n_distinct(id),
#             acc = accuracy(home_win, round(home_prob))) |> 
#   bind_rows(
#     test |> 
#       mutate(playoff_round = "overall") |> 
#       mutate(id = paste0(season, "-", series_id)) |> 
#       group_by(playoff_round) |>
#       summarise(n_games = n_distinct(id),
#                 acc = accuracy(home_win, round(home_prob)))
#   )
# 





