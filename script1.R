#wokring for train.default
require(dplyr)
require(unbalanced)
require(randomForest)
require(h2o)
require(caret) #main lib
require(magrittr)
require(tidyquant)


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



#change_factor_level <- function(data) {
#  data %>%
#    mutate(went_on_backorder = as.factor(as.numeric(went_on_backorder)-2)) %>%
#    mutate(rev_stop = as.factor(as.numeric(rev_stop)-2)) %>%
#    mutate(stop_auto_buy = as.factor(as.numeric(stop_auto_buy)-2)) %>%
#    mutate(ppap_risk = as.factor(as.numeric(ppap_risk)-2)) %>%
#    mutate(oe_constraint = as.factor(as.numeric(oe_constraint)-2)) %>%
#    mutate(deck_risk = as.factor(as.numeric(deck_risk)-2)) %>%
#    mutate(potential_issue = as.factor(as.numeric(potential_issue)-2))
  
#}

#train <- change_factor_level(train)
#test <- change_factor_level(test)

##change able






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
names(train)

X<-train[, -c(22)]    

# Exclude Id and target

#    Get y as a factor NOT 1,2 as now but 0 and 1 as ubSMOTE needs binary Y as input.


y<-as.factor(as.numeric(train[, 22])-2) 

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

train <- cbind(b_data$X, went_on_backorder = b_data$Y)



table(train$went_on_backorder)/nrow(train)

nrow(train)

names(train)




#make every colume numeric

#train$deck_risk <- as.numeric(train$deck_risk) -2
train$deck_risk <- as.numeric(train$deck_risk)+2
train$potential_issue <- as.numeric(train$potential_issue) -2
train$oe_constraint <- as.numeric(train$oe_constraint) -2
train$ppap_risk <- as.numeric(train$ppap_risk) -2
train$stop_auto_buy <- as.numeric(train$stop_auto_buy) -2
train$rev_stop <- as.numeric(train$rev_stop) -2
train$went_on_backorder <- as.numeric(train$went_on_backorder)+1 





str(train) 


#principal component analysis
prin_comp <- prcomp(train, center = TRUE, scale. = T)
names(prin_comp)

spine_PCA <- prcomp(train, center = TRUE, scale. = T)

#center = mean, and scale = standard deviation
####################################################
head(spine_PCA$rotation, 5)

spine_PCA_summary <- summary(spine_PCA)

# Generate the PCA standard deviations
spinePCA_SD<-spine_PCA$sdev

# Generate the PCA variances
spinePCA_var <- round(spinePCA_SD^2, digits = 3)

# Generate the proportion of variance for each component
propvar_spinePCA <- spinePCA_var / sum(spinePCA_var)


# Produce a screeplot and a barplot of the amount of variance each component contributes.
plot(spine_PCA_summary$importance[2, 1:22], xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "h")

barplot(spine_PCA_summary$importance[2, 1:22], xlab = "Principal Component", ylab = "Proportion of Variance Explained")














#######################################################
#outputs the mean of variables
prin_comp$center


#outputs the standard deviation of variables
prin_comp$scale


prin_comp$rotation


prin_comp$rotation[1:22,1:22]


dim(prin_comp$x)


biplot(prin_comp, scale = 0)

plot(prin_comp)


#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]



#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]


#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")



#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")



#add a training set with principal components
train.data <- data.frame(went_on_backorder = train$went_on_backorder, prin_comp$x)

View(train.data)

#we are interested in first 30 PCAs
#train.data <- train.data[,1:31]

#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model <- rpart(went_on_backorder ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA

test.data <- predict(prin_comp, newdata = test)

test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:22]

#make prediction on test data
rpart.prediction <- predict(rpart.model,  test.data)

#
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)





###################################################################

fit <- princomp(train, cor=FALSE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
dev.copy(pdf,"yo.pdf")
biplot(fit)
dev.off()
