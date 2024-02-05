library(tidyverse)
library(ggpubr)

# load in the dataframe showing each team-season's win-loss streak over the regular season
df <- read.csv("C:/Users/jeffr/Documents/spring 2024/data blog idea/nfl streaks/nfl_team_level_wins2002to2019.csv")

# reformatting df to create heatmaps
df_final <- df %>% 
  mutate(last_week = 17 + make_playoffs + make_divisional_game + make_championship_game + 
           make_super_bowl + win_super_bowl
         ) %>% 
  pivot_longer(
    cols = game1:game16,
    names_to = "game",
    values_to = "outcome"
  ) %>% 
  mutate(
    game = recode(game, game1 = 1, game2 = 2, game3 = 3, game4 = 4, game5 = 5, game6 = 6, game7 = 7, 
                  game8 = 8, game9 = 9, game10 = 10, game11 = 11, game12 = 12, game13 = 13, 
                  game14 = 14, game15 = 15, game16 = 16 
                  ),
    outcome = recode(outcome, W = 1, tie = 0, L = -1)
      ) %>% 
  group_by(X) %>% 
  mutate(rolling_win_loss_diff = cumsum(outcome),
         final_regular_season_differential = last(rolling_win_loss_diff)
         ) %>% 
  ungroup() 

# write function to generate heatmap for each season (year)
plot_generator <- function(year){
  p <- df_final %>% 
    group_by(season) %>% 
    select(season, team, last_week, final_regular_season_differential) %>% 
    distinct() %>% 
    arrange(desc(last_week), desc(final_regular_season_differential)) %>% 
    mutate(chart_ranking = 1:32) %>%  
    ungroup() %>% 
    select(season, team, chart_ranking) %>% 
    merge(., df_final) %>% 
    filter(season == year) %>% 
    ggplot(aes(x = factor(game), y = reorder(team, -chart_ranking), 
               fill = rolling_win_loss_diff)) + 
    geom_tile() + 
    scale_fill_gradientn(colours = rev(heat.colors(33))) + 
    geom_hline(yintercept = 20.5, linewidth = 1)  + 
    annotate(geom="text", x=1.5, y=19.5, label="missed playoffs",
             color="black", hjust=0) + 
    annotate(geom="text", x=1.5, y=21.5, label="made playoffs",
             color="black", hjust=0) + 
    geom_segment(aes(x = 1, y = 10, xend = 1, yend = 15), size=1,
                 arrow = arrow(length = unit(0.5, "cm"))
    ) + 
    geom_segment(aes(x = 1, y = 25, xend = 1, yend = 30), size=1,
                 arrow = arrow(length = unit(0.5, "cm"))
    ) +
    annotate(geom="text", x=1.5, y=12.5, label="more total reg. season wins",
             color="black", hjust=0) + 
    annotate(geom="text", x=1.5, y=27.5, label="more total playoff wins",
             color="black", hjust=0) + 
    labs(x = "Regular season game", y = "Team", fill = "Momentum",
         title = "NFL Regular Season Momentum and Playoff Performance",
         subtitle = paste(year, "Season"))
  return(p)
}

# generate and save plots
plot_generator(2002)
plot_generator(2003)
plot_generator(2004)
plot_generator(2005)
plot_generator(2006)
plot_generator(2007)
plot_generator(2008)
plot_generator(2009)
plot_generator(2010)
plot_generator(2011)
plot_generator(2012)
plot_generator(2013)
plot_generator(2014)
plot_generator(2015)
plot_generator(2016)
plot_generator(2017)
plot_generator(2018)
plot_generator(2019)
