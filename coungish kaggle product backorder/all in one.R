#installing
install.packages(c("randomForest","caret","pROC","ROSE","knitr","magrittr","gridExtra","GGally","rpart.plot","BaylorEdPsych","dplyr","ggplot2","gplots","grid","gridExtra",
                   "DT","GGally","rpart","caret","pROC","ROSE","magrittr","tidyr","rpart.plot"
                   ,"tibble","ROCR","Amelia","pscl","mvnmle","ggcorrplot","reshape2","DataExplorer","e1071"))

#importing the libs
library(h2o)
library(tidyquant)
library(unbalanced)
library(dplyr)
library(ggplot2)
library(gplots)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)
library(rpart)
library(caret)
library(pROC)
library(ROSE)
library(magrittr)
library(tidyr)
library(rpart.plot)
library(tibble)
library(ROCR)
library(purrr)
library(Amelia)
library(magrittr)
library(ggcorrplot)
library(reshape2)
library(DataExplorer)
library(e1071)
library(pscl)
#input data


raw_train <- read.csv("~/Kaggle_Training_Dataset_v2.csv")

raw_test <- read.csv("~/Kaggle_Test_Dataset_v2.csv")

#basic analysis on train

str(raw_train)
head(raw_train)
raw_train$went_on_backorder %>% table() %>% prop.table()

# Examine the testing dataset
str(raw_test)
head(raw_test)
raw_test$went_on_backorder %>% table() %>% prop.table()

glimpse(raw_train)


#Showing the data imbalance and showing its visual data

qplot( as.factor(raw_train$went_on_backorder) ) + 
  geom_bar() + 
  labs(x="went_on_backorder", y="Count")

#Showing the data with missing values and will work on it now Amelia
missmap(raw_train)


missmap(raw_test)

preprocess_raw_data <- function(data) {
  # data = data frame of backorder data
  data[data == -99] <- NA
  data %>%
    select(-sku) %>%
    mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) 
  
}

preprocess_data <- function(data) {
  
  data %>% mutate(deck_risk = as.factor(deck_risk)) %>% 
    mutate(oe_constraint = as.factor(oe_constraint)) %>% 
    mutate(ppap_risk = as.factor(ppap_risk)) %>% 
    mutate(stop_auto_buy = as.factor(stop_auto_buy)) %>% 
    mutate(rev_stop = as.factor(rev_stop)) %>%
    mutate(went_on_backorder = as.factor(went_on_backorder))
}



# Applying the function created above to training set

transform_train_1 <- preprocess_raw_data(raw_train)

transform_test_1 <- preprocess_raw_data(raw_test)

# Correlation matrix some error in cor

str(transform_train_1)
corr <- round(cor(transform_train_1, use = "pairwise.complete.obs"), 2)
tail(corr)
melted_cormat <- melt(corr, na.rm = TRUE)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile()
ggcorrplot(corr)

transform_train <- preprocess_data(transform_train_1)
transform_test <- preprocess_data(transform_test_1)


#checking again

missmap(transform_train)
missmap(transform_test)

#visualization of data 

plot_missing(transform_train)

plot_missing(transform_test)

#since the missing values in ´lead time´, ´perf_12_month_avg´ and ´perf_6_month_avg´ are more than 5% of both training and testing sets so it's quite safe to just ignore these variables by droping the entire columns out of the datasets 

clean_function <- function(data) {
  
  data %>% 
    select(-lead_time, -perf_12_month_avg , -perf_6_month_avg ) 
  
  
}

clean_train <- clean_function(transform_train)
clean_test <- clean_function(transform_test)
plot_missing(clean_train)
any(is.na(clean_train))


str(clean_train)

#M1 under and over sampleing

train_balanced_both <- ovun.sample(went_on_backorder~ ., data=clean_train,p=0.5, seed=1, N = 500000, method="both")$data 

table(train_balanced_both$went_on_backorder) # showing result after both under and oversampling

#M2 under sampling


train_balanced_under <- ovun.sample(went_on_backorder ~ ., data = clean_train, method = "under", N = 20000, seed = 1)$data

table(train_balanced_under$went_on_backorder) # Showing the result after undersamling 


#visualization of the above thing

qplot( as.factor(train_balanced_both$went_on_backorder) ) + 
  geom_bar() + 
  labs(x="went_on_backorder", y="Count")
any(is.na(train_balanced_both))
any(is.na(train_balanced_under))


#modelling the data
sim_tree <- rpart(formula = went_on_backorder ~ . , 
                  data = train_balanced_both, 
                  method = "class",
                  parms = list(split='information'), 
                  control = rpart.control(maxdepth = 5, minsplit = 70, cp = 0.0014 ))

rpart.plot(sim_tree)

#prediction on the model

# Generate predicted classes using the model object
class_prediction <- predict(sim_tree, transform_test, type = "class")  

# Calculate the confusion matrix for the test set not working need check the reason
confusionMatrix(class_prediction, transform_test$went_on_backorder)


#working and checking ROC curve

h2o.init(nthreads = -1, max_mem_size = '16g', ip = "127.0.0.1", port = 54321)



h2o.no_progress()

# Convert to H2OFrame as h20o package doesnt work with dataframe
train_h2o <- as.h2o(clean_train)
test_h2o <- as.h2o(clean_test)

# Automatic Machine Learning
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 45
)
