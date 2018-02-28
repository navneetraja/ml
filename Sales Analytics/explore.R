library("unbalanced");
library("tidyquant");
library("h2o");


#inputing the dataset 
train_raw_df  <- read.csv("Kaggle_Training_Dataset_v2.csv");
test_raw_df   <- read.csv("Kaggle_Test_Dataset_v2.csv");

#analysing the data

#checking -99  and Na in diffrent columns
train_raw_df %>% head() %>% knitr::kable()


#result for last NA row
train_raw_df %>% tail() %>% knitr::kable()

#checking of balance of data

# Unbalanced data set
train_raw_df$went_on_backorder %>% table() %>% prop.table()

#insepecing missing values

#train set: Percentage of complete cases
train_raw_df %>% complete.cases() %>% sum() / nrow(train_raw_df)


# test set: Percentage of complete cases
test_raw_df %>% complete.cases() %>% sum() / nrow(test_raw_df)


#showing glimpse of all data
glimpse(train_raw_df)

#for assitance in modeling we need valid dataset so from the training dataset lets create 85/15 ration of valid dataset

split_pct <- 0.85
n <- nrow(train_raw_df)
sample_size <- floor(split_pct * n)


set.seed(159)
idx_train <- sample(1:n, size = sample_size)

valid_raw_df <- train_raw_df[-idx_train,]
train_raw_df <- train_raw_df[idx_train,]

#need to do many trails on above section

