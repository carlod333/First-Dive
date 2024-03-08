library(tidyverse)
library(rddensity)
library(rdrobust)

# chess data taken from the following kaggle page: https://www.kaggle.com/datasets/datasnaek/chess
df <- read.csv(
  "chess_games.csv"
  ) %>% 
  mutate(y = as.numeric(winner=="white"),
         x = white_rating - black_rating
  ) %>% 
  filter(x != 0)

attach(df)

#  distribution of x 
df %>% 
  ggplot(aes(x=x, fill=factor(x>0))) + 
  geom_histogram() + 
  geom_vline(xintercept=0) + 
  xlim(-100,100) +
  theme_bw()+
  theme(legend.position='none') + 
  labs(x = "Difference", y = "Count",
       title = "Distribution of differences between white and black ratings",
       caption = "Differences between -100 and 100"
       )


# RDD analysis
summary(rdrobust(y=y, x=x, all=TRUE))

rdplot(y=y, x=x,  ci=95,
       nbins=10,
       title="RD Plot: Chess Data",
       y.label="Proportion of games won by white",
       x.label="Difference between white and black ratings")
