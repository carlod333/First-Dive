# part one: data merging, visualizations, simple models
library(tidyverse)
library(ggpubr)
library(ngramr)

# merging the google ngram and crime data ----
ngram_data <- ngram(
  c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits"), 
  year_start = 1960,
  year_end = 2019,
  corpus = "en-US-2019" 
  ) %>% 
  pivot_wider(names_from=Phrase, values_from=Frequency) %>% 
  rename(year=Year)


crime_data <- read.csv("yearly_us_crimerates_per100k.csv",
                       header=T, stringsAsFactors=FALSE, fileEncoding="latin1")
names(crime_data) <- c("year", "pop", "total", "violent", "property", "murder", "forcible_rape",
                       "robbery", "aggravated_assault", "burglary", "larceny_theft", "vehicle_theft")
crime_data$year <- as.numeric(str_trim(crime_data$year))
crime_data$pop <- as.numeric(str_trim(crime_data$pop))
crime_data$total <- as.numeric(str_trim(crime_data$total))
crime_data$violent <- as.numeric(str_trim(crime_data$violent))
crime_data$property <- as.numeric(str_trim(crime_data$property))
crime_data$murder <- as.numeric(str_trim(crime_data$murder))
crime_data$forcible_rape <- as.numeric(str_trim(crime_data$forcible_rape))
crime_data$robbery <- as.numeric(str_trim(crime_data$robbery))
crime_data$aggravated_assault <- as.numeric(str_trim(crime_data$aggravated_assault))
crime_data$burglary <- as.numeric(str_trim(crime_data$burglary))
crime_data$larceny_theft <- as.numeric(str_trim(crime_data$larceny_theft))
crime_data$vehicle_theft <- as.numeric(str_trim(crime_data$vehicle_theft))

df <- merge(crime_data, ngram_data, by="year") %>% 
  mutate(violent_lagged = lag(violent, 1),
         violent_mean_past_value = cummean(violent)
         ) 

# write to csv for analysis in python
write.csv(df, "ngram_crime_data.csv")

# visualizations of each variable ----

df %>% 
  ggplot(aes(x = year, y = violent)) + 
  geom_line() + 
  labs(x = "Year", y = "Violent crimes per 100 thousand",
       title = "U.S. violent crime rate per 100 thousand people (1960-2019)")

p1 <- df %>% 
  ggplot(aes(x = year, y = shit)) + 
  geom_line() + 
  labs(x = "")

p2 <- df %>% 
  ggplot(aes(x = year, y = piss)) + 
  geom_line() + 
  labs(x = "")

p3 <- df %>% 
  ggplot(aes(x = year, y = fuck)) + 
  geom_line()+ 
  labs(x = "")

p4 <- df %>% 
  ggplot(aes(x = year, y = cunt)) + 
  geom_line()+ 
  labs(x = "")

p5 <- df %>% 
  ggplot(aes(x = year, y = cocksucker)) + 
  geom_line()+ 
  labs(x = "")

p6 <- df %>% 
  ggplot(aes(x = year, y = motherfucker)) + 
  geom_line()+ 
  labs(x = "")

p7 <- df %>% 
  ggplot(aes(x = year, y = tits)) + 
  geom_line()+ 
  labs(x = "")

ggarrange(p1, p2, p3, p4, p5, p6, p7,
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 3) %>% 
  annotate_figure(., top = text_grob("The seven words you can't say on TV: frequencies from 1960-2019", 
                                        color = "black", face = "bold", size = 14))

# results for linear model (including train test split) ----
mae <- function(preds, actual){
  errors <- preds - actual
  value <- mean(abs(errors), na.rm=T)
  return(value)
}

rmse <- function(preds, actual){
  squared_errors <- (preds - actual)^2
  value <- sqrt( mean(squared_errors, na.rm=T) )
  return(value)
}


train <- df %>% filter(year < 2008)
test <- df %>% filter(year >= 2008)

lm_train <- lm(violent ~ shit + piss + fuck + cunt + cocksucker + 
                 motherfucker + tits, 
               data=train)


mae(predict(lm_train, newdata=test), test$violent)
rmse(predict(lm_train, newdata=test), test$violent)

# simple prediction methods ----

# results, using test set, from random walk model
mae(test$violent_lagged, test$violent)
rmse(test$violent_lagged, test$violent)

# results, using test set, from past mean value
mae(test$violent_mean_past_value, test$violent)
rmse(test$violent_mean_past_value, test$violent)

