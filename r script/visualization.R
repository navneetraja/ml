#combining both the datasets

df_all <- rbind(train,test)

#Checking for balanced data and taking action accordingly


table(df_all$went_on_backorder)/nrow(df_all)

#i dont know
#Balance dataset

X<-df_all[, -c(1,23)]    

# Exclude Id and target

#    Get y as a factor NOT 1,2 as now but 0 and 1 as ubSMOTE needs binary Y as input.


y<-as.factor(as.numeric(df_all[, 23])-2) 

#Balancing

b_data <- ubSMOTE(X = X, Y = y,   # Also y be a vector not a dataframe
                  perc.over=200,   #  200/100 = 2 instances generated for every rare instance
                  perc.under=500,  #  500/100 = 5 instances selected for every smoted observation
                  k=3,
                  verbose=TRUE) 


# ubSMOTE returns balanced data frame in b_data$X
#      and corresponding class values, as vector in b_data$Y
#       Return value 'b_data' itself is a list-structure
#     So complete and balanced train data is:

df_alldata <- cbind(b_data$X, label = b_data$Y)

dim(df_alldata)


table(df_alldata$label)/nrow(df_alldata)

