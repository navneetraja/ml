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

# Import data in h2oFrame converting into h2o frame

s_file<-as.h2o(df_alldata)

class(s_file)

# Check the data in file

dim(s_file)

head(s_file,1)

# Show all data objects on the H2O platform

h2o.ls()

NN_model = h2o.deeplearning(
  x = 1:21,                # Last column is label
  training_frame = s_file,
  hidden = c(10, 5, 4, 5, 10 ),    # Autoencoder layers
  epochs = 600,
  activation = "Tanh",
  autoencoder = TRUE
)


#Extract the non-linear feature from layer 3 of H2O data set
#using an H2O deep learning model
df_all_supervised_features2 = h2o.deepfeatures(NN_model, s_file, layer=3)


head(df_all_supervised_features2) # Note column names, Layer3.Col1, Layer3.Col2


#Make a dataframe of 2+1 columns
df = as.data.frame(df_all_supervised_features2)  # 2 columns
df$label = as.character(as.vector(s_file[,22])) # +1 column
dim(df)

#plotting all the constrains
p1 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C2)) + geom_point(aes(col=label))
p2 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C3)) + geom_jitter(aes(col=label))
p3 = ggplot(df, aes(x= DF.L3.C1, y = DF.L3.C4)) + geom_point(aes(col=label))
p4 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C1)) + geom_jitter(aes(col=label))
p5 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C3)) + geom_jitter(aes(col=label))
p6 = ggplot(df, aes(x= DF.L3.C2, y = DF.L3.C4)) + geom_jitter(aes(col=label))
p7 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C1)) + geom_jitter(aes(col=label))
p8 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C2)) + geom_jitter(aes(col=label))
p9 = ggplot(df, aes(x= DF.L3.C3, y = DF.L3.C4)) + geom_jitter(aes(col=label))
p10 = ggplot(df, aes(x= DF.L3.C4, y = DF.L3.C1)) + geom_jitter(aes(col=label))
p11 = ggplot(df, aes(x= DF.L3.C4, y = DF.L3.C2)) + geom_jitter(aes(col=label))
p12 = ggplot(df, aes(x= DF.L3.C4, y = DF.L3.C3)) + geom_jitter(aes(col=label))

dev.copy(pdf,"cluster_1.pdf")
plot_grid(p1, p2, p3,
          labels = c("C1,2", "C1,3", "C1,4"))
dev.off()

dev.copy(pdf,"cluster_2.pdf")
plot_grid(p4, p5, p6,
          labels = c("C2,1", "C2,3", "C2,4"))
dev.off()

dev.copy(pdf,"cluster_3.pdf")
plot_grid(p7, p8, p9,
          labels = c("C3,1", "C3,2", "C3,4"))
dev.off()

dev.copy(pdf,"cluster_4.pdf")
plot_grid(p10, p11, p12,
          labels = c("C4,1", "C4,2", "C4,3"))
dev.off()

dev.copy(pdf,"all clusters.pdf")
plot_grid(p1, p2, p3,p4, p5, p6,p7, p8, p9,p10, p11, p12,
          labels = c("C1,2", "C1,3", "C1,4","C2,1", "C2,3", "C2,4","C3,1", "C3,2", "C3,4","C4,1", "C4,2", "C4,3"))
dev.off()


h2o.shutdown()


