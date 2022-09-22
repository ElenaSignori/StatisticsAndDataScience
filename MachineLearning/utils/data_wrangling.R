library(readr)
library(lubridate)
library(tidyverse)
library(rsample)
library(recipes)
library(caret)

source("utils/make_dummy.R")

SeoulBikeData <- read_csv("data/SeoulBikeData.csv")

names(SeoulBikeData) <- c("date",              "rented_bike_count",
                          "hour",              "temperature",            
                          "humidity",          "wind_speed",       
                          "visibility",        "dew_point_temperature",  
                          "solar_radiation",   "rainfall",           
                          "snowfall",          "seasons",                
                          "holiday",           "functioning_day")

SeoulBikeData <- SeoulBikeData %>% 
  mutate(
    date = dmy(date),
    seasons = as.factor(seasons),
    holiday = as.factor(holiday), 
    functioning_day = as.factor(functioning_day)
  ) 

# Prendi i nomi (abbreviati) dei giorni e dei mesi dalla data
weekday <- format(SeoulBikeData$date, format = "%a")
month <- format(SeoulBikeData$date, format = "%b")

SeoulBikeData$weekday <- as.factor(weekday)
SeoulBikeData$month <- as.factor(month)

SeoulBikeData <- SeoulBikeData %>% 
  mutate(
    weekend = as.factor(ifelse(weekday %in% c("Fri", "Sat", "Sun"), 
                               "Yes", "No"))
  ) %>% 
  select(-date) # Rimuovo 'date' perché trattiamo i dati come cross-section

#### Split training set + validation set, test set ####
set.seed(123) 
split <- initial_split(SeoulBikeData, prop = 0.9)
bike_train_full <- training(split)
bike_test <- testing(split)

#### Split training set, validation set ####
set.seed(123) 
split <- initial_split(bike_train_full, prop = 0.8)
bike_train <- training(split)
bike_valid <- testing(split)

#### Trasformo variabili categoriche in dummy ####
to_dummy <- c("seasons", "holiday" ,"functioning_day", 
       "weekday", "month", "weekend")

bike_train_dummy <- make_dummy(bike_train, rented_bike_count ~ ., to_dummy)
bike_test_dummy <- make_dummy(bike_test, rented_bike_count ~ ., to_dummy)
bike_valid_dummy <- make_dummy(bike_valid, rented_bike_count ~ ., to_dummy)


#### Esporto tutto in csv ####
write.csv(bike_train, "data/bike_train.csv", row.names = FALSE)
write.csv(bike_test, "data/bike_test.csv", row.names = FALSE)
write.csv(bike_valid, "data/bike_valid.csv", row.names = FALSE)

write.csv(bike_train_dummy, "data/bike_train_dummy.csv", row.names = FALSE)
write.csv(bike_test_dummy, "data/bike_test_dummy.csv", row.names = FALSE)
write.csv(bike_valid_dummy, "data/bike_valid_dummy.csv", row.names = FALSE)

# Pulisci environment
rm(weekday, month, split, SeoulBikeData, bike_train_full,
   to_dummy, make_dummy)







