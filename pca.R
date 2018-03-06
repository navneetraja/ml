# Read in the data
spine_dat <- read.csv("Kaggle_Training_Dataset_v2.csv", header = TRUE)

str(spine_dat)
#Re-label the class column

spine_dat$deck_risk <- as.numeric(spine_dat$deck_risk)-2
spine_dat$potential_issue <- as.numeric(spine_dat$potential_issue) -2
spine_dat$oe_constraint <- as.numeric(spine_dat$oe_constraint) -2
spine_dat$ppap_risk <- as.numeric(spine_dat$ppap_risk) -2
spine_dat$stop_auto_buy <- as.numeric(spine_dat$stop_auto_buy) -2
spine_dat$rev_stop <- as.numeric(spine_dat$rev_stop) -2
spine_dat$went_on_backorder <- as.numeric(spine_dat$went_on_backorder) -2

#train$deck_risk <- as.numeric(train$deck_risk) -2
#spine_dat$Class_att <- as.numeric(spine_dat$Class_att)-1
#spine_dat$Class_att[spine_dat$Class_att == "Abnormal"] <- 1
#spine_dat$Class_att[spine_dat$Class_att == "Normal"] <- 2

# Conduct the pca using the prcom() function. Center will center all variables, and scaling 
# will standardize all the variables.
str(spine_dat)
spine_PCA <- prcomp(spine_dat, center = TRUE, scale. = TRUE)

# Retrieve the first 5 prinicipal component loadings.
head(spine_PCA$rotation, 5)

spine_PCA_summary <- summary(spine_PCA)

# Generate the PCA standard deviations
spinePCA_SD<-spine_PCA$sdev

# Generate the PCA variances
spinePCA_var <- round(spinePCA_SD^2, digits = 3)

# Generate the proportion of variance for each component
propvar_spinePCA <- spinePCA_var / sum(spinePCA_var)


# Produce a screeplot and a barplot of the amount of variance each component contributes.
plot(spine_PCA_summary$importance[2, 1:12], xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "h")

barplot(spine_PCA_summary$importance[2, 1:12], xlab = "Principal Component", ylab = "Proportion of Variance Explained")

# Conduct Horn's Parallel Analysis
library("paran")
horn<-paran(spine_dat, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = FALSE, seed = 0)
