require(dplyr)
require(unbalanced)
require(randomForest)
require(h2o)
require(caret) #main lib
require(magrittr)
require(tidyquant)
require(class)
require(caret)

train<-read.csv("~/Kaggle_Training_Dataset_v2.csv")
test<-read.csv("~/Kaggle_Test_Dataset_v2.csv")



#checking the dataset
dim(train)

dim(test)

#heading some rows of it and checking metadata using names fx
head(train)

head(test)

names(train)

names(test)   


#using summary for basic analysis
summary(train)

summary(test)


#basic analysis using str
str(train)

str(test)

#phase for data processing


#selected
preprocess_raw_data <- function(data) {
  # data = data frame of backorder data
  data[data == -99] <- NA
  data %>%
    select(-sku) #%>%
  #mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) 
}

train <- preprocess_raw_data(train) 
test  <- preprocess_raw_data(test)

# How many NA values are there checking
sum(is.na(train))

sum(is.na(test))



#intension to remove NA
#Inner apply() reduces each value in column to TRUE or FALSE further
#The outer apply(), sums up column wise and gives output

apply(apply(train,2,is.na),2,sum) ; nrow(train)  

apply(apply(test,2,is.na),2,sum) ; nrow(test)


#problem with the last row all are NA so
#Removing last row from train and test data as that seems to be number of rows and an invalid row.
tail(train,1)
tail(test,1)


train_count <- nrow(train) 

test_count <- nrow(test)

train_count

test_count

train<-train[-train_count,]

test<-test[-test_count,]

apply(apply(train,2,is.na),2,sum) ; nrow(train)  

apply(apply(test,2,is.na),2,sum) ; nrow(test)


train %<>% na.roughfix()
test %<>% na.roughfix()

#Check for any further NA if there exist
sum(is.na(train))

sum(is.na(test))

str(train)
str(test)


#balancing the dataset



names(test)

X<-train[, -c(13)]    

str(x)
dim(x)
# Exclude Id and target

#    Get y as a factor NOT 1,2 as now but 0 and 1 as ubSMOTE needs binary Y as input.


y<-as.factor(as.numeric(train[, 13])-2) 

#Balancing

b_data <- ubSMOTE(X = X, Y = y,   # Also y be a vector not a dataframe
                  perc.over=200,   #  200/100 = 2 instances generated for every rare instance
                  perc.under=200,  #  500/100 = 5 instances selected for every smoted observation
                  k=5,
                  verbose=TRUE) 


# ubSMOTE returns balanced data frame in b_data$X
#      and corresponding class values, as vector in b_data$Y
#       Return value 'b_data' itself is a list-structure
#     So complete and balanced train data is:

train<- cbind(b_data$X, went_on_backorder = b_data$Y)

str(train)


table(train$went_on_backorder)/nrow(train)

nrow(train)

names(train)


#######################reducting some dimensions##########################

backorder <- function(data) {
  # data = data frame of backorder data
  data %>%
    mutate(went_on_backorder = as.factor(as.numeric(went_on_backorder)-1))
  #mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) 
}

str(train)

train$went_on_backorder <- (as.numeric(train$went_on_backorder)-1)

#train$went_on_backorder <- as.factor(train$went_on_backorder)

str(train)



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

prc_n <- as.data.frame(lapply(train, normalize))

