
library(rvest)


pg <- read_html("https://www.sportsoddshistory.com/mlb-playoffs-series/?y=0000&o=s&fv=&hv=&fd=&rd=")

playoff_odds <- pg |> html_elements(".soh1") |> html_table() |> data.frame()

playoff_odds <- playoff_odds |> 
  rename(home=Team.withhome.field..seed.,
         away = Team.w.ohome.field..seed.) |>
  janitor::clean_names() |> 
  mutate(home = gsub(" \\(.*", "", home),
         away = gsub(" \\(.*", "", away)) |> 
  mutate(playoff_round = case_when(
    str_detect(round, "WCS") ~ "1st",
    str_detect(round, "LDS") ~ "2nd",
    str_detect(round, "LCS") ~ "3rd",
    str_detect(round, "WS") ~ "4th"
  ))

odds_converted <- function(moneyline) {
  # if(moneyline < 0) {
  #   p <- (- (-moneyline) / ((- (-moneyline)) + 100))
  # } else {
  #   p <- 100 / (moneyline + 100)
  # }
  p <- dplyr::case_when(
    moneyline < 0 ~ (moneyline*-1) / ((moneyline*-1) + 100),
    moneyline > 0 ~ 100 / (moneyline + 100)
  )
  return(p)
}



playoff_odds <- playoff_odds |> 
  mutate(home_prob_betting = odds_converted(team_withhome_fieldodds),
         away_prob_betting = odds_converted(team_w_ohome_fieldodds)) |> 
  mutate(home_prob_vig_free = home_prob_betting / (home_prob_betting + away_prob_betting),
         away_prob_vig_free = away_prob_betting / (home_prob_betting + away_prob_betting))



playoff_odds <- playoff_odds |> 
  select(season, playoff_round, home, away, home_prob_vig_free, away_prob_vig_free, winning_team, series_score) |> 
  mutate(season = as.character(season)) |> 
  mutate(
    home = case_when(
      home == "St Louis Cardinals" ~ "St. Louis Cardinals",
      TRUE ~ home
    ),
    away = case_when(
      away == "St Louis Cardinals" ~ "St. Louis Cardinals",
      TRUE ~ away
    ),
    winning_team = case_when(
      winning_team == "St Louis Cardinals" ~ "St. Louis Cardinals",
      TRUE ~ winning_team
    )
  ) |> 
  filter(season >= 1990) |> 
  arrange(season, playoff_round)



playoff_odds_long <- playoff_odds |> 
  select(season, playoff_round, team=home, prob_vig_free=home_prob_vig_free) |> 
  bind_rows(
    playoff_odds |> 
      select(season, playoff_round, team=away, prob_vig_free=away_prob_vig_free)
  )




zz <- analysis_df |> 
  mutate(playoff_round = case_when(
    str_detect(series_id, "F_|R_") ~ "1st",
    str_detect(series_id, "D_") ~ "2nd",
    str_detect(series_id, "L_") ~ "3rd",
    str_detect(series_id, "W_") ~ "4th"
  )) |> 
  left_join(playoff_odds_long, by = c("season", "playoff_round", "teams_home_team_name" = "team")) |> 
  left_join(playoff_odds_long, by = c("season", "playoff_round", "teams_away_team_name" = "team"), suffix = c("_home", "_away"))





zz |> 
  filter(is.na(prob_vig_free_home)) |> 
  distinct(season, series_id, playoff_round, teams_home_team_name, teams_away_team_name) |> 
  view()















