#phase for data processing


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


#good to go

