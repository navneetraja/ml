library(dummies)
library(class)
library(gmodels)
library(ggplot2)
library(dplyr)
library(unbalanced)
library(randomForest)
library(magrittr)
library(tidyquant)
library(C50)

orders_train <- read.csv("~/Kaggle_Training_Dataset_v2.csv")

orders_test <- read.csv("~/Kaggle_Test_Dataset_v2.csv")


#cleaning the data

str(orders_train)

summary(orders_train)


train_count <- nrow(orders_train)

test_count <- nrow(orders_test)


train<-orders_train[-train_count,]

test<-orders_test[-test_count,]

apply(apply(orders_train,2,is.na),2,sum) ; nrow(orders_train)  

apply(apply(orders_test,2,is.na),2,sum) ; nrow(orders_test)


orders_train %<>% na.roughfix()
orders_test %<>% na.roughfix()


#removing 99
preprocess_raw_data <- function(data) {
  # data = data frame of backorder data
  data %>%
    select(-sku) %>%
    drop_na(national_inv) %>%
    mutate(lead_time = ifelse(is.na(lead_time), -99, lead_time)) %>%
    mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) %>%
    mutate(went_on_backorder = as.factor(went_on_backorder))
}


p_train <- preprocess_raw_data(orders_train) 
p_test  <- preprocess_raw_data(orders_test)



colSums(is.na(orders_train))
#smorting

input  <- p_train %>% select(-went_on_backorder)

output <- as.factor(as.numeric(p_train$went_on_backorder)-2)

train_balanced <- ubSMOTE(input, output, perc.over = 200, perc.under = 200, k = 5 , verbose = TRUE)

p_train <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))


#distribution for prediction variable

round(prop.table(table(p_train$went_on_backorder)) * 100, 1)


backorder_model <- C5.0(p_train[ , -22], p_train$went_on_backorder)
