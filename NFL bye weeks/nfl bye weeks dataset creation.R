library(tidyverse)
#library(dplyr)
library(nflreadr)

# loading the NFL play-by-play data
df_scores <- load_pbp(1999:2023) %>% 
  select(season, week, season_type, home_team, away_team, home_score, away_score) %>%
  distinct() 

# optional: save df_scores as csv df_scores for future analysis (to save a few minutes)
#write.csv(df_scores, "nflscores1999to2023.csv")
#df_scores <- read.csv("nflscores1999to2023.csv")

df_scores_transformed <- df_scores %>% 
  select(-X) %>% 
  .[rep(seq_len(nrow(.)), each = 2), ] %>% 
  mutate(team = 
           ifelse(row_number() %% 2 == 0, 
                  away_team, home_team
           ),
         opponent = 
           ifelse(row_number() %% 2 != 0, 
                  away_team, home_team
           ),
         team_score = 
           ifelse(
             home_team==team, home_score, away_score
           ),
         opponent_score = 
           ifelse(
             home_team==team, away_score, home_score
           ),
         team_is_home = 
           ifelse( home_team==team, "home", "away"),
         team_won = as.numeric(team_score > opponent_score),
         team_point_differential = team_score - opponent_score
  ) %>% 
  select(-(home_team:away_score)) %>% 
  group_by(season, team) %>% 
  mutate(week_of_last_game_played = max(week)) %>% 
  ungroup() %>% 
  filter(season_type=="REG") 

bye_weeks_by_team <- df_scores_transformed %>% 
  complete(season, week, team) %>%
  arrange(-desc(team)) %>% 
  filter(is.na(season_type), week < 17) %>% 
  mutate(nonexistent = as.numeric(team=="HOU" & week==16 & season %in% 1999:2001)) %>% 
  filter(nonexistent == 0) %>% 
  select(season, team, bye_week=week) %>% 
  group_by(season) %>% 
  mutate(latest_bye_week_by_season = max(bye_week)) 

df_scores_transformed_final <- df_scores_transformed %>% 
  merge(., bye_weeks_by_team) %>% 
  mutate(team_week_first_treated = bye_week + 1) %>% 
  mutate(team_week_first_treated = ifelse(bye_week==latest_bye_week_by_season, 0, team_week_first_treated)
  ) %>% 
  group_by(team) %>% 
  mutate(team_id=cur_group_id()
         ) %>% 
  ungroup() %>% 
  group_by(season, week) %>% 
  mutate(season_week_id=cur_group_id()) %>% 
  ungroup() 

# this is the csv that will be used in the next R file (and in analyses presented in the article) 
write.csv(df_scores_transformed_final, "nflscores_transformed_final.csv")
