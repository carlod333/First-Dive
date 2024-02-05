library(tidyverse)
library(nflreadr)



# took 5-10 minutes on my laptop
# load in NFL game-by-game data from 2002-2019
df_scores <- load_pbp(2002:2019) %>%
  select(season, week, season_type, home_team, away_team, home_score, away_score) %>%
  distinct() %>% 
  mutate(home_team_diff = home_score - away_score)

# save df_scores as csv for future analysis
#write.csv(df_scores, "nfl_scores2002to2019.csv")
#df_scores <- read.csv("nfl_scores2002to2019.csv")

# create new dataframe showing each team-season's win-loss streak over the regular season
df_final <- expand.grid(team = unique(df_scores$home_team), season = unique(df_scores$season))
df_final[, c("game1", "game2", "game3", "game4", "game5", "game6", "game7", "game8", 
             "game9", "game10", "game11", "game12", "game13", "game14", "game15", "game16",
             "make_playoffs", "make_divisional_game", "make_championship_game", "make_super_bowl",
             "win_super_bowl")
         ] <- NA

for (i in 1:nrow(df_final)){
  team_score_margins <- df_scores %>% 
    filter(season == df_final$season[i],  
           season_type=="REG", 
           home_team == df_final$team[i] | away_team == df_final$team[i]) %>% 
    mutate(team_of_interest_diff = ifelse(home_team==df_final$team[i], home_team_diff, 
                                          ifelse(
                                            away_team==df_final$team[i] & home_team_diff >= 0,
                                            -1*home_team_diff,
                                            abs(home_team_diff)
                                          )
    )
    ) %>% 
    pull(team_of_interest_diff) 
  
  df_final[i, c("game1", "game2", "game3", "game4", "game5", "game6", "game7", "game8", 
                "game9", "game10", "game11", "game12", "game13", "game14", "game15", "game16")
  ] <- team_score_margins
  
  home_playoff_teams <- df_scores %>% 
    filter(season==df_final$season[i], season_type=="POST") %>% 
    pull(home_team)
  away_playoff_teams <- df_scores %>% 
    filter(season==df_final$season[i], season_type=="POST") %>% 
    pull(away_team)
  
  home_divisional_playoff_teams <- df_scores %>% 
    filter(season==df_final$season[i], week==19, season_type=="POST") %>% 
    pull(home_team)
  away_divisional_playoff_teams <-  df_scores %>% 
    filter(season==df_final$season[i], week==19, season_type=="POST") %>% 
    pull(away_team)
  
  home_championship_playoff_teams <-  df_scores %>% 
    filter(season==df_final$season[i], week==20, season_type=="POST") %>% 
    pull(home_team)
  away_championship_playoff_teams <- df_scores %>% 
    filter(season==df_final$season[i], week==20, season_type=="POST") %>% 
    pull(away_team)
  
  home_super_bowl_team <- df_scores %>% 
    filter(season==df_final$season[i], week==21, season_type=="POST") %>% 
    pull(home_team)
  away_super_bowl_team <- df_scores %>% 
    filter(season==df_final$season[i], week==21, season_type=="POST") %>% 
    pull(away_team)
  
  super_bowl_winning_team <- df_scores %>% 
    filter(season==df_final$season[i], week==21) %>% 
    mutate(winner = ifelse(home_score > away_score, home_team, away_team)) %>% 
    pull(winner)
  
  df_final[i, "make_playoffs"] <- as.numeric(df_final$team[i] %in% c(home_playoff_teams, away_playoff_teams))
  df_final[i, "make_divisional_game"] <- as.numeric(df_final$team[i] %in% c(home_divisional_playoff_teams, away_divisional_playoff_teams))
  df_final[i, "make_championship_game"] <- as.numeric(df_final$team[i] %in% c(home_championship_playoff_teams, away_championship_playoff_teams))
  df_final[i, "make_super_bowl"] <- as.numeric(df_final$team[i] %in% c(home_super_bowl_team, away_super_bowl_team))
  df_final[i, "win_super_bowl"] <- as.numeric(df_final$team[i]==super_bowl_winning_team)
  
  
    
}

df_wins <- df_final
for (i in 1:nrow(df_wins)){
  for (j in c("game1", "game2", "game3", "game4", "game5", "game6", "game7", "game8", 
              "game9", "game10", "game11", "game12", "game13", "game14", "game15", "game16")
  ){
    df_wins[i, j]<- ifelse(df_wins[i, j] > 0, "W", 
                               ifelse(df_wins[i, j]==0, "tie", "L"))
  }
}

#save df_wins as csv for future analysis
#write.csv(df_wins, "nfl_team_level_wins2002to2019.csv")