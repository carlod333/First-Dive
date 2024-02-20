set.seed(333)
library(tidyverse)
library(panelView)
library(did)

# loading in df_scores_transformed_final from the dataset creation R file
df = read.csv("nflscores_transformed_final.csv")

# plot of bye weeks for NFL teams in 2019 regular season
df %>% 
  filter(season==2019) %>% 
  arrange(-desc(team), -desc(week)) %>% 
  mutate(treatment = as.numeric(week >= bye_week),
         group = bye_week,
         game_number = rep_len(1:16, length.out=nrow(.)),
         game_number_is_treated = as.numeric(week > bye_week)
         ) %>% 
  panelview(data = ., team_won ~ game_number_is_treated, index = c("team", "game_number"),
          type= "treat", outcome.type = "discrete", treat.type = "discrete",
          pre.post=TRUE, by.timing=TRUE,
          xlab =  "Game number", ylab = "Team",
          main = "Each team's bye week during the 2019 regular season"
          )

df %>% 
  filter(season %in% 2002:2019) %>% 
  select(season, team, bye_week) %>% 
  distinct() %>% 
  ggplot(aes(x = season, y = fct_rev(team), fill = bye_week)) + 
  geom_tile() + 
  scale_fill_gradientn(colours = rev(heat.colors(13)),
                       breaks = c(1, 7, 13),
                       name = "bye week"
                       ) + 
  labs(x = "Season", y = "Team", title = "Bye weeks by team (2002-2019)"
       )  

df %>% 
  filter(season %in% 2002:2019) %>% 
  select(season, team, bye_week) %>% 
  distinct() %>% 
  group_by(season, bye_week) %>% 
  summarize(n = n()) %>% 
  mutate(season = factor(season), bye_week = factor(bye_week)) %>% 
  ggplot(aes(x = season, y = fct_relevel(bye_week, as.character(1:13)), fill = n)) + 
  geom_tile() + 
  scale_fill_gradientn(colours = rev(heat.colors(4)), name="frequency") + 
  labs(x = "Season", y = "Bye week", title = "Frequency of bye weeks (2002-2019)"
  ) 


# preparing the dataset for the DID (plus creating the two covariates: opponent quality and team home-v-away status)
df_final <- df %>% 
  filter(season %in% 2002:2019) %>% 
  mutate(team_season = paste0(team, "-", season)) %>% 
  arrange(-desc(season), -desc(team), -desc(week)) %>% 
  mutate(treatment = as.numeric(week >= bye_week),
         group = bye_week,
         game_number = rep_len(1:16, length.out=nrow(.)),
         game_number_is_treated = as.numeric(week > bye_week)
  ) %>% 
  filter(game_number <=11) %>% 
  mutate(team_season_never_treated = as.numeric(bye_week > 11)) %>% 
  group_by(team_season) %>% 
  mutate(team_season_id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(first_game_when_team_is_treated = ifelse(team_season_never_treated==1, 0, bye_week),
         season = factor(season),
         made_playoffs = as.numeric(week_of_last_game_played > 17)) %>% 
  filter( !(first_game_when_team_is_treated %in% c(1,2)) ) %>% 
  mutate(opponent_team_season = paste0(opponent, "-", season)) %>% 
  mutate(opponent_point_differential = opponent_score - team_score) %>% 
  group_by(opponent_team_season) %>%
  mutate(opponent_point_differential_rolling = cumsum(opponent_point_differential)) %>% 
  ungroup()

# running the DID model using the Callaway and Sant'Anna package (did)
nfl.attgt <- att_gt(yname = "team_point_differential",
                    gname = "first_game_when_team_is_treated", # need first game when a team is treated
                    idname = "team_season_id",
                    tname = "game_number",
                    xformla = ~ opponent_point_differential_rolling + team_is_home, 
                    data = df_final)

# plot the results
ggdid(nfl.attgt, group=3:5, title = "ATT estimates: teams with byes in weeks 3-5",
      grtitle = "bye week", xlab = "game number", ylab = "team point differential")
ggdid(nfl.attgt, group=6:8, title = "ATT estimates: teams with byes in weeks 6-8",
      grtitle = "bye week", xlab = "game number", ylab = "team point differential")
ggdid(nfl.attgt, group=9:11, title = "ATT estimates: teams with byes in weeks 9-11",
      grtitle = "bye week", xlab = "game number", ylab = "team point differential")

