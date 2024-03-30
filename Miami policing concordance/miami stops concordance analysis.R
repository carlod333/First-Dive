library(tidyverse)
library(sandwich)
library(lmtest)

# below running this file, first download the csv below obtained from the .ipynb file

df <- read.csv("miami_dade_county_stops.csv")

# cleaning the data for doing linear regressions ----
df <- df %>% 
  mutate(
    year = substr(date, start = 1, stop = 4),
    month = substr(date, start = 6, stop = 7),
    hour = substr(time, start = 1, stop = 2),
    arrest_flag = ifelse(arrest_made=="True", 1, 
                         ifelse(
                           arrest_made=="False", 0,
                           NA
                         )
                         ),
    citation_flag = ifelse(citation_issued=="True", 1, 
                         ifelse(
                           citation_issued=="False", 0,
                           NA
                         )
                         ),
    warning_flag = ifelse(warning_issued=="True", 1, 
                          ifelse(
                            warning_issued=="False", 0,
                            NA
                          )
                          ),
    frisk_flag = ifelse(frisk_performed=="True", 1, 
                        ifelse(
                          frisk_performed=="False", 0,
                          NA
                        )
                        ),
    search_flag = ifelse(search_conducted=="True", 1, 
                         ifelse(
                           search_conducted=="False", 0,
                           NA
                         )
                         ),
    subject_black = as.numeric(subject_race=="black"),
    officer_black = as.numeric(officer_race=="black")
  ) %>%
  select(year:hour, subject_age, subject_black, subject_sex, officer_age, officer_black, 
         officer_sex, arrest_flag:search_flag) %>% 
  na.omit() %>% 
  .[apply(., 1, function(row) !any(row == "")), ]

# gender concordance regressions (outcome: arrest_flag) ----

# linear probability models
linear_short_gender_arrest <- df %>% 
  lm(arrest_flag ~ subject_sex*officer_sex, data=.) 

cov_linear_short_gender_arrest <- vcovHC(linear_short_gender_arrest, type = "HC1")
robust_se_linear_short_gender_arrest <- sqrt(diag(cov_linear_short_gender_arrest))


linear_long_gender_arrest <- df %>% 
  lm(arrest_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
       officer_age + officer_black, 
     data=.) 

cov_linear_long_gender_arrest <- vcovHC(linear_long_gender_arrest, type = "HC1")
robust_se_linear_long_gender_arrest <- sqrt(diag(cov_linear_long_gender_arrest))




# logit models
logit_short_gender_arrest <- df %>% 
  glm(arrest_flag ~ subject_sex*officer_sex, data=., family = "binomial") 

cov_logit_short_gender_arrest <- vcovHC(logit_short_gender_arrest, type = "HC1")
robust_se_logit_short_gender_arrest <- sqrt(diag(cov_logit_short_gender_arrest))

logit_long_gender_arrest <- df %>% 
  glm(arrest_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = "binomial")

cov_logit_long_gender_arrest <- vcovHC(logit_long_gender_arrest, type = "HC1")
robust_se_logit_long_gender_arrest <- sqrt(diag(cov_logit_long_gender_arrest))


  
# probit models
probit_short_gender_arrest <- df %>% 
  glm(arrest_flag ~ subject_sex*officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_short_gender_arrest <- vcovHC(probit_short_gender_arrest, type = "HC1")
robust_se_probit_short_gender_arrest <- sqrt(diag(cov_probit_short_gender_arrest))

probit_long_gender_arrest <- df %>% 
  glm(arrest_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = binomial(link = "probit")) 

cov_probit_long_gender_arrest <- vcovHC(probit_long_gender_arrest, type = "HC1")
robust_se_probit_long_gender_arrest <- sqrt(diag(cov_probit_long_gender_arrest))

# gender concordance regressions (outcome: citation_flag) ----

# linear probability models
linear_short_gender_citation <- df %>% 
  lm(citation_flag ~ subject_sex*officer_sex, data=.) 

cov_linear_short_gender_citation <- vcovHC(linear_short_gender_citation, type = "HC1")
robust_se_linear_short_gender_citation <- sqrt(diag(cov_linear_short_gender_citation))


linear_long_gender_citation <- df %>% 
  lm(citation_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
       officer_age + officer_black, 
     data=.) 

cov_linear_long_gender_citation <- vcovHC(linear_long_gender_citation, type = "HC1")
robust_se_linear_long_gender_citation <- sqrt(diag(cov_linear_long_gender_citation))




# logit models
logit_short_gender_citation <- df %>% 
  glm(citation_flag ~ subject_sex*officer_sex, data=., family = "binomial") 

cov_logit_short_gender_citation <- vcovHC(logit_short_gender_citation, type = "HC1")
robust_se_logit_short_gender_citation <- sqrt(diag(cov_logit_short_gender_citation))

logit_long_gender_citation <- df %>% 
  glm(citation_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = "binomial")

cov_logit_long_gender_citation <- vcovHC(logit_long_gender_citation, type = "HC1")
robust_se_logit_long_gender_citation <- sqrt(diag(cov_logit_long_gender_citation))



# probit models
probit_short_gender_citation <- df %>% 
  glm(citation_flag ~ subject_sex*officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_short_gender_citation <- vcovHC(probit_short_gender_citation, type = "HC1")
robust_se_probit_short_gender_citation <- sqrt(diag(cov_probit_short_gender_citation))

probit_long_gender_citation <- df %>% 
  glm(citation_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = binomial(link = "probit")) 

cov_probit_long_gender_citation <- vcovHC(probit_long_gender_citation, type = "HC1")
robust_se_probit_long_gender_citation <- sqrt(diag(cov_probit_long_gender_citation))


# gender concordance regressions (outcome: warning_flag) ----

# linear probability models
linear_short_gender_warning <- df %>% 
  lm(warning_flag ~ subject_sex*officer_sex, data=.) 

cov_linear_short_gender_warning <- vcovHC(linear_short_gender_warning, type = "HC1")
robust_se_linear_short_gender_warning <- sqrt(diag(cov_linear_short_gender_warning))


linear_long_gender_warning <- df %>% 
  lm(warning_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
       officer_age + officer_black, 
     data=.) 

cov_linear_long_gender_warning <- vcovHC(linear_long_gender_warning, type = "HC1")
robust_se_linear_long_gender_warning <- sqrt(diag(cov_linear_long_gender_warning))




# logit models
logit_short_gender_warning <- df %>% 
  glm(warning_flag ~ subject_sex*officer_sex, data=., family = "binomial") 

cov_logit_short_gender_warning <- vcovHC(logit_short_gender_warning, type = "HC1")
robust_se_logit_short_gender_warning <- sqrt(diag(cov_logit_short_gender_warning))

logit_long_gender_warning <- df %>% 
  glm(warning_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = "binomial")

cov_logit_long_gender_warning <- vcovHC(logit_long_gender_warning, type = "HC1")
robust_se_logit_long_gender_warning <- sqrt(diag(cov_logit_long_gender_warning))



# probit models
probit_short_gender_warning <- df %>% 
  glm(warning_flag ~ subject_sex*officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_short_gender_warning <- vcovHC(probit_short_gender_warning, type = "HC1")
robust_se_probit_short_gender_warning <- sqrt(diag(cov_probit_short_gender_warning))

probit_long_gender_warning <- df %>% 
  glm(warning_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = binomial(link = "probit")) 

cov_probit_long_gender_warning <- vcovHC(probit_long_gender_warning, type = "HC1")
robust_se_probit_long_gender_warning <- sqrt(diag(cov_probit_long_gender_warning))

# gender concordance regressions (outcome: frisk_flag) ----
# linear probability models
linear_short_gender_frisk <- df %>% 
  lm(frisk_flag ~ subject_sex*officer_sex, data=.) 

cov_linear_short_gender_frisk <- vcovHC(linear_short_gender_frisk, type = "HC1")
robust_se_linear_short_gender_frisk <- sqrt(diag(cov_linear_short_gender_frisk))


linear_long_gender_frisk <- df %>% 
  lm(frisk_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
       officer_age + officer_black, 
     data=.) 

cov_linear_long_gender_frisk <- vcovHC(linear_long_gender_frisk, type = "HC1")
robust_se_linear_long_gender_frisk <- sqrt(diag(cov_linear_long_gender_frisk))




# logit models
logit_short_gender_frisk <- df %>% 
  glm(frisk_flag ~ subject_sex*officer_sex, data=., family = "binomial") 

cov_logit_short_gender_frisk <- vcovHC(logit_short_gender_frisk, type = "HC1")
robust_se_logit_short_gender_frisk <- sqrt(diag(cov_logit_short_gender_frisk))

logit_long_gender_frisk <- df %>% 
  glm(frisk_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = "binomial")

cov_logit_long_gender_frisk <- vcovHC(logit_long_gender_frisk, type = "HC1")
robust_se_logit_long_gender_frisk <- sqrt(diag(cov_logit_long_gender_frisk))



# probit models
probit_short_gender_frisk <- df %>% 
  glm(frisk_flag ~ subject_sex*officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_short_gender_frisk <- vcovHC(probit_short_gender_frisk, type = "HC1")
robust_se_probit_short_gender_frisk <- sqrt(diag(cov_probit_short_gender_frisk))

probit_long_gender_frisk <- df %>% 
  glm(frisk_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = binomial(link = "probit")) 

cov_probit_long_gender_frisk <- vcovHC(probit_long_gender_frisk, type = "HC1")
robust_se_probit_long_gender_frisk <- sqrt(diag(cov_probit_long_gender_frisk))


# gender concordance regressions (outcome: search_flag) ----

linear_short_gender_search <- df %>% 
  lm(search_flag ~ subject_sex*officer_sex, data=.) 

cov_linear_short_gender_search <- vcovHC(linear_short_gender_search, type = "HC1")
robust_se_linear_short_gender_search <- sqrt(diag(cov_linear_short_gender_search))


linear_long_gender_search <- df %>% 
  lm(search_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
       officer_age + officer_black, 
     data=.) 

cov_linear_long_gender_search <- vcovHC(linear_long_gender_search, type = "HC1")
robust_se_linear_long_gender_search <- sqrt(diag(cov_linear_long_gender_search))




# logit models
logit_short_gender_search <- df %>% 
  glm(search_flag ~ subject_sex*officer_sex, data=., family = "binomial") 

cov_logit_short_gender_search <- vcovHC(logit_short_gender_search, type = "HC1")
robust_se_logit_short_gender_search <- sqrt(diag(cov_logit_short_gender_search))

logit_long_gender_search <- df %>% 
  glm(search_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = "binomial")

cov_logit_long_gender_search <- vcovHC(logit_long_gender_search, type = "HC1")
robust_se_logit_long_gender_search <- sqrt(diag(cov_logit_long_gender_search))



# probit models
probit_short_gender_search <- df %>% 
  glm(search_flag ~ subject_sex*officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_short_gender_search <- vcovHC(probit_short_gender_search, type = "HC1")
robust_se_probit_short_gender_search <- sqrt(diag(cov_probit_short_gender_search))

probit_long_gender_search <- df %>% 
  glm(search_flag ~ subject_sex*officer_sex + year + month + hour + subject_age + subject_black + 
        officer_age + officer_black, data=., family = binomial(link = "probit")) 

cov_probit_long_gender_search <- vcovHC(probit_long_gender_search, type = "HC1")
robust_se_probit_long_gender_search <- sqrt(diag(cov_probit_long_gender_search))
# race concordance regressions (outcome: arrest_flag) ----

# linear probability models
linear_short_race_arrest <- df %>% 
  lm(arrest_flag ~ subject_black*officer_black, data=.) 

cov_linear_short_race_arrest <- vcovHC(linear_short_race_arrest, type = "HC1")
robust_se_linear_short_race_arrest <- sqrt(diag(cov_linear_short_race_arrest))


linear_long_race_arrest <- df %>% 
  lm(arrest_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
       officer_age + officer_sex, 
     data=.) 

cov_linear_long_race_arrest <- vcovHC(linear_long_race_arrest, type = "HC1")
robust_se_linear_long_race_arrest <- sqrt(diag(cov_linear_long_race_arrest))




# logit models
logit_short_race_arrest <- df %>% 
  glm(arrest_flag ~ subject_black*officer_black, data=., family = "binomial") 

cov_logit_short_race_arrest <- vcovHC(logit_short_race_arrest, type = "HC1")
robust_se_logit_short_race_arrest <- sqrt(diag(cov_logit_short_race_arrest))

logit_long_race_arrest <- df %>% 
  glm(arrest_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = "binomial")

cov_logit_long_race_arrest <- vcovHC(logit_long_race_arrest, type = "HC1")
robust_se_logit_long_race_arrest <- sqrt(diag(cov_logit_long_race_arrest))



# probit models
probit_short_race_arrest <- df %>% 
  glm(arrest_flag ~ subject_black*officer_black, data=., family = binomial(link = "probit")) 

cov_probit_short_race_arrest <- vcovHC(probit_short_race_arrest, type = "HC1")
robust_se_probit_short_race_arrest <- sqrt(diag(cov_probit_short_race_arrest))

probit_long_race_arrest <- df %>% 
  glm(arrest_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_long_race_arrest <- vcovHC(probit_long_race_arrest, type = "HC1")
robust_se_probit_long_race_arrest <- sqrt(diag(cov_probit_long_race_arrest))

# race concordance regressions (outcome: citation_flag) ----

# linear probability models
linear_short_race_citation <- df %>% 
  lm(citation_flag ~ subject_black*officer_black, data=.) 

cov_linear_short_race_citation <- vcovHC(linear_short_race_citation, type = "HC1")
robust_se_linear_short_race_citation <- sqrt(diag(cov_linear_short_race_citation))


linear_long_race_citation <- df %>% 
  lm(citation_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
       officer_age + officer_sex, 
     data=.) 

cov_linear_long_race_citation <- vcovHC(linear_long_race_citation, type = "HC1")
robust_se_linear_long_race_citation <- sqrt(diag(cov_linear_long_race_citation))




# logit models
logit_short_race_citation <- df %>% 
  glm(citation_flag ~ subject_black*officer_black, data=., family = "binomial") 

cov_logit_short_race_citation <- vcovHC(logit_short_race_citation, type = "HC1")
robust_se_logit_short_race_citation <- sqrt(diag(cov_logit_short_race_citation))

logit_long_race_citation <- df %>% 
  glm(citation_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = "binomial")

cov_logit_long_race_citation <- vcovHC(logit_long_race_citation, type = "HC1")
robust_se_logit_long_race_citation <- sqrt(diag(cov_logit_long_race_citation))



# probit models
probit_short_race_citation <- df %>% 
  glm(citation_flag ~ subject_black*officer_black, data=., family = binomial(link = "probit")) 

cov_probit_short_race_citation <- vcovHC(probit_short_race_citation, type = "HC1")
robust_se_probit_short_race_citation <- sqrt(diag(cov_probit_short_race_citation))

probit_long_race_citation <- df %>% 
  glm(citation_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_long_race_citation <- vcovHC(probit_long_race_citation, type = "HC1")
robust_se_probit_long_race_citation <- sqrt(diag(cov_probit_long_race_citation))


# race concordance regressions (outcome: warning_flag) ----

# linear probability models
linear_short_race_warning <- df %>% 
  lm(warning_flag ~ subject_black*officer_black, data=.) 

cov_linear_short_race_warning <- vcovHC(linear_short_race_warning, type = "HC1")
robust_se_linear_short_race_warning <- sqrt(diag(cov_linear_short_race_warning))


linear_long_race_warning <- df %>% 
  lm(warning_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
       officer_age + officer_sex, 
     data=.) 

cov_linear_long_race_warning <- vcovHC(linear_long_race_warning, type = "HC1")
robust_se_linear_long_race_warning <- sqrt(diag(cov_linear_long_race_warning))




# logit models
logit_short_race_warning <- df %>% 
  glm(warning_flag ~ subject_black*officer_black, data=., family = "binomial") 

cov_logit_short_race_warning <- vcovHC(logit_short_race_warning, type = "HC1")
robust_se_logit_short_race_warning <- sqrt(diag(cov_logit_short_race_warning))

logit_long_race_warning <- df %>% 
  glm(warning_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = "binomial")

cov_logit_long_race_warning <- vcovHC(logit_long_race_warning, type = "HC1")
robust_se_logit_long_race_warning <- sqrt(diag(cov_logit_long_race_warning))



# probit models
probit_short_race_warning <- df %>% 
  glm(warning_flag ~ subject_black*officer_black, data=., family = binomial(link = "probit")) 

cov_probit_short_race_warning <- vcovHC(probit_short_race_warning, type = "HC1")
robust_se_probit_short_race_warning <- sqrt(diag(cov_probit_short_race_warning))

probit_long_race_warning <- df %>% 
  glm(warning_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_long_race_warning <- vcovHC(probit_long_race_warning, type = "HC1")
robust_se_probit_long_race_warning <- sqrt(diag(cov_probit_long_race_warning))

# race concordance regressions (outcome: frisk_flag) ----
# linear probability models
linear_short_race_frisk <- df %>% 
  lm(frisk_flag ~ subject_black*officer_black, data=.) 

cov_linear_short_race_frisk <- vcovHC(linear_short_race_frisk, type = "HC1")
robust_se_linear_short_race_frisk <- sqrt(diag(cov_linear_short_race_frisk))


linear_long_race_frisk <- df %>% 
  lm(frisk_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
       officer_age + officer_sex, 
     data=.) 

cov_linear_long_race_frisk <- vcovHC(linear_long_race_frisk, type = "HC1")
robust_se_linear_long_race_frisk <- sqrt(diag(cov_linear_long_race_frisk))




# logit models
logit_short_race_frisk <- df %>% 
  glm(frisk_flag ~ subject_black*officer_black, data=., family = "binomial") 

cov_logit_short_race_frisk <- vcovHC(logit_short_race_frisk, type = "HC1")
robust_se_logit_short_race_frisk <- sqrt(diag(cov_logit_short_race_frisk))

logit_long_race_frisk <- df %>% 
  glm(frisk_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = "binomial")

cov_logit_long_race_frisk <- vcovHC(logit_long_race_frisk, type = "HC1")
robust_se_logit_long_race_frisk <- sqrt(diag(cov_logit_long_race_frisk))



# probit models
probit_short_race_frisk <- df %>% 
  glm(frisk_flag ~ subject_black*officer_black, data=., family = binomial(link = "probit")) 

cov_probit_short_race_frisk <- vcovHC(probit_short_race_frisk, type = "HC1")
robust_se_probit_short_race_frisk <- sqrt(diag(cov_probit_short_race_frisk))

probit_long_race_frisk <- df %>% 
  glm(frisk_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_long_race_frisk <- vcovHC(probit_long_race_frisk, type = "HC1")
robust_se_probit_long_race_frisk <- sqrt(diag(cov_probit_long_race_frisk))


# race concordance regressions (outcome: search_flag) ----

linear_short_race_search <- df %>% 
  lm(search_flag ~ subject_black*officer_black, data=.) 

cov_linear_short_race_search <- vcovHC(linear_short_race_search, type = "HC1")
robust_se_linear_short_race_search <- sqrt(diag(cov_linear_short_race_search))


linear_long_race_search <- df %>% 
  lm(search_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
       officer_age + officer_sex, 
     data=.) 

cov_linear_long_race_search <- vcovHC(linear_long_race_search, type = "HC1")
robust_se_linear_long_race_search <- sqrt(diag(cov_linear_long_race_search))




# logit models
logit_short_race_search <- df %>% 
  glm(search_flag ~ subject_black*officer_black, data=., family = "binomial") 

cov_logit_short_race_search <- vcovHC(logit_short_race_search, type = "HC1")
robust_se_logit_short_race_search <- sqrt(diag(cov_logit_short_race_search))

logit_long_race_search <- df %>% 
  glm(search_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = "binomial")

cov_logit_long_race_search <- vcovHC(logit_long_race_search, type = "HC1")
robust_se_logit_long_race_search <- sqrt(diag(cov_logit_long_race_search))



# probit models
probit_short_race_search <- df %>% 
  glm(search_flag ~ subject_black*officer_black, data=., family = binomial(link = "probit")) 

cov_probit_short_race_search <- vcovHC(probit_short_race_search, type = "HC1")
robust_se_probit_short_race_search <- sqrt(diag(cov_probit_short_race_search))

probit_long_race_search <- df %>% 
  glm(search_flag ~ subject_black*officer_black + year + month + hour + subject_age + subject_sex + 
        officer_age + officer_sex, data=., family = binomial(link = "probit")) 

cov_probit_long_race_search <- vcovHC(probit_long_race_search, type = "HC1")
robust_se_probit_long_race_search <- sqrt(diag(cov_probit_long_race_search))

# summarizing the regression output ----

outcome_types <- c("arrested", "citation", "frisked", "searched", "warning")
model_types <- c("LPM", "logistic", "probit")
gender_tau_results <- data.frame(outcome = rep(rep(outcome_types, length(model_types)), 2), 
                                 model = rep(c(rep(model_types[1], length(outcome_types)),
                                           rep(model_types[2], length(outcome_types)),
                                           rep(model_types[3], length(outcome_types))
                                           ), 2),
                                 predictors = c(rep("long", length(outcome_types)*length(model_types)),
                                                rep("short", length(outcome_types)*length(model_types))
                                                ),
                                 tau_est = c(summary(linear_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         
                                         summary(logit_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         
                                         summary(probit_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         
                                         summary(linear_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(linear_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         
                                         summary(logit_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(logit_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         
                                         summary(probit_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Estimate"],
                                         summary(probit_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Estimate"]
                                         
                                         ),
                                 tau_se = c(summary(linear_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            
                                            summary(logit_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            
                                            summary(probit_long_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_long_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_long_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_long_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_long_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            
                                            summary(linear_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(linear_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            
                                            summary(logit_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(logit_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            
                                            summary(probit_short_gender_arrest)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_short_gender_citation)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_short_gender_frisk)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_short_gender_search)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"],
                                            summary(probit_short_gender_warning)$coefficients["subject_sexmale:officer_sexmale", "Std. Error"]
                                            
                                 )
                                
                                 ) %>% 
  mutate(ci95containszero = ifelse(0 >= tau_est - 1.96*tau_se & 0 <= tau_est + 1.96*tau_se,
                                   1, 0
  )
  ) %>% 
  unite("model_specific", outcome:predictors, sep = " ", remove = FALSE)

gender_tau_results %>% 
  ggplot(aes(x = fct_reorder(model_specific, desc(model_specific)), 
             y = tau_est, color = ifelse(ci95containszero==0, "red", "black"))
         ) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') + 
  geom_errorbar(aes(ymin = tau_est - 1.96*tau_se, ymax = tau_est + 1.96*tau_se), width = 0.2) +
  coord_flip(ylim = c(-5, 5)) + 
  scale_color_identity() + 
  labs(y = "tau estimate", x = "",
       title = "Gender concordance regressions",
       subtitle = "Tau estimates with 95 percent CIs")
  
  

race_tau_results <- data.frame(outcome = rep(rep(outcome_types, length(model_types)), 2), 
                                 model = rep(c(rep(model_types[1], length(outcome_types)),
                                               rep(model_types[2], length(outcome_types)),
                                               rep(model_types[3], length(outcome_types))
                                 ), 2),
                                 predictors = c(rep("long", length(outcome_types)*length(model_types)),
                                                rep("short", length(outcome_types)*length(model_types))
                                 ),
                                 tau_est = c(summary(linear_long_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_long_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_long_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_long_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_long_race_warning)$coefficients["subject_black:officer_black", "Estimate"],
                                             
                                             summary(logit_long_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_long_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_long_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_long_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_long_race_warning)$coefficients["subject_black:officer_black", "Estimate"],
                                             
                                             summary(probit_long_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_long_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_long_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_long_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_long_race_warning)$coefficients["subject_black:officer_black", "Estimate"],
                                             
                                             summary(linear_short_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_short_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_short_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_short_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(linear_short_race_warning)$coefficients["subject_black:officer_black", "Estimate"],
                                             
                                             summary(logit_short_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_short_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_short_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_short_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(logit_short_race_warning)$coefficients["subject_black:officer_black", "Estimate"],
                                             
                                             summary(probit_short_race_arrest)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_short_race_citation)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_short_race_frisk)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_short_race_search)$coefficients["subject_black:officer_black", "Estimate"],
                                             summary(probit_short_race_warning)$coefficients["subject_black:officer_black", "Estimate"]
                                             
                                 ),
                                 tau_se = c(summary(linear_long_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_long_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_long_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_long_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_long_race_warning)$coefficients["subject_black:officer_black", "Std. Error"],
                                            
                                            summary(logit_long_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_long_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_long_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_long_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_long_race_warning)$coefficients["subject_black:officer_black", "Std. Error"],
                                            
                                            summary(probit_long_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_long_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_long_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_long_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_long_race_warning)$coefficients["subject_black:officer_black", "Std. Error"],
                                            
                                            summary(linear_short_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_short_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_short_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_short_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(linear_short_race_warning)$coefficients["subject_black:officer_black", "Std. Error"],
                                            
                                            summary(logit_short_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_short_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_short_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_short_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(logit_short_race_warning)$coefficients["subject_black:officer_black", "Std. Error"],
                                            
                                            summary(probit_short_race_arrest)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_short_race_citation)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_short_race_frisk)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_short_race_search)$coefficients["subject_black:officer_black", "Std. Error"],
                                            summary(probit_short_race_warning)$coefficients["subject_black:officer_black", "Std. Error"]
                                            
                                 )
                                 
) %>% 
  mutate(ci95containszero = ifelse(0 >= tau_est - 1.96*tau_se & 0 <= tau_est + 1.96*tau_se,
                                   1, 0
  )
  ) %>% 
  unite("model_specific", outcome:predictors, sep = " ", remove = FALSE)

race_tau_results %>% 
  ggplot(aes(x = fct_reorder(model_specific, desc(model_specific)), 
             y = tau_est, color = ifelse(ci95containszero==0, "red", "black"))
  ) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') + 
  geom_errorbar(aes(ymin = tau_est - 1.96*tau_se, ymax = tau_est + 1.96*tau_se), width = 0.2) +
  coord_flip(ylim = c(-5, 5)) + 
  scale_color_identity() + 
  labs(y = "tau estimate", x = "",
       title = "Race concordance regressions",
       subtitle = "Tau estimates with 95 percent CIs")
