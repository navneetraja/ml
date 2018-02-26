#importing libraries
library(ggplot2)
library(h2o)
library(unbalanced)
library(dplyr)
library(randomForest)
library(magrittr)
library(cowplot)


h2o.init()


#taking input for product backorder
training_dataset_path <- readline(prompt="Training Path ")
testing_dataset_path <- readline(prompt="Testing Path ")



train<-read.csv(training_dataset_path)
test<-read.csv(testing_dataset_path)

