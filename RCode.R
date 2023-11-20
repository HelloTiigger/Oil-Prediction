rm(list=ls())
("readxl")
library(data.table)
library(caTools)            # Train-Test Split
library(rpart)              # CART
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(e1071)
library(knitr)
library(car)
library(gridExtra)


#Import Data:
petroleum = read.csv("petroleum.csv")
# Change 'DOWNHOLE' to 'BOTTOMHOLE':
colnames(petroleum)[c(3,4)] =c( "AVG_BOTTOMHOLE_PRESSURE","AVG_BOTTOMHOLE_TEMPERATURE")
petroleum$DATEPRD <- parse_date_time(petroleum$DATEPRD, orders = c("dmy", "mdy", "ymd"))

# Create a summary table
summary_table <- summary(petroleum)
summary_table

# Create boxplots for each variable
boxplot1 <- ggplot(petroleum, aes( y = AVG_BOTTOMHOLE_PRESSURE)) +
  geom_boxplot() +labs(title = "")
boxplot2 <- ggplot(petroleum, aes( y = AVG_BOTTOMHOLE_TEMPERATURE)) +
  geom_boxplot() +
  labs(title = "")
boxplot3 <- ggplot(petroleum, aes( y = AVG_DP_TUBING)) +
  geom_boxplot() +
  labs(title = "")
boxplot4 <- ggplot(petroleum, aes( y = AVG_CHOKE_SIZE_P)) +
  geom_boxplot() +
  labs(title = "")
boxplot5 <- ggplot(petroleum, aes( y = AVG_WHP_P)) +
  geom_boxplot() +
  labs(title = "")
boxplot6 <- ggplot(petroleum, aes( y = AVG_WHT_P)) +
  geom_boxplot() +
  labs(title = "")
boxplot7 <- ggplot(petroleum, aes( y = DP_CHOKE_SIZE)) +
  geom_boxplot() +
  labs(title = "")
boxplot8 <- ggplot(petroleum, aes( y = BORE_OIL_VOL)) +
  geom_boxplot() +
  labs(title = "")
# Combine boxplots into one view side by side
grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, boxplot5, boxplot6, boxplot7, boxplot8, ncol = 4)

# Plot BORE_OIL_VOL time trend
plot <- ggplot(petroleum, aes(x = DATEPRD, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "Date", y = "BORE_OIL_VOL") +
  ggtitle("")

# Create a boxplot for BORE_OIL_VOL
ggplot(petroleum, aes(y = BORE_OIL_VOL)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of BORE_OIL_VOL",
    y = "BORE_OIL_VOL"
  ) +
  theme_minimal()
# No. of outliers
outliers <- boxplot.stats(petroleum$BORE_OIL_VOL)$out

# Calculate the quartiles and IQR
Q1 <- quantile(petroleum$BORE_OIL_VOL, 0.25)
Q3 <- quantile(petroleum$BORE_OIL_VOL, 0.75)
IQR <- Q3 - Q1

# Plot BORE_OIL_VOL against Independent Variables
plot1 <- ggplot(petroleum, aes(x = AVG_BOTTOMHOLE_PRESSURE, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_BOTTOMHOLE_PRESSURE", y = "BORE_OIL_VOL") +
  ggtitle("")
plot2 <- ggplot(petroleum, aes(x = AVG_BOTTOMHOLE_TEMPERATURE, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_BOTTOMHOLE_TEMPERATURE", y = "BORE_OIL_VOL") +
  ggtitle("")
plot3 <- ggplot(petroleum, aes(x = AVG_DP_TUBING, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_DP_TUBING", y = "BORE_OIL_VOL") +
  ggtitle("")
plot4 <- ggplot(petroleum, aes(x = AVG_CHOKE_SIZE_P, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_CHOKE_SIZE_P", y = "BORE_OIL_VOL") +
  ggtitle("")
plot5 <- ggplot(petroleum, aes(x = AVG_WHP_P, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_WHP_P", y = "BORE_OIL_VOL") +
  ggtitle("")
plot6 <- ggplot(petroleum, aes(x = AVG_WHT_P, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "AVG_WHT_P", y = "BORE_OIL_VOL") +
  ggtitle("")
plot7 <- ggplot(petroleum, aes(x = DP_CHOKE_SIZE, y = BORE_OIL_VOL)) +
  geom_point() +
  labs(x = "DP_CHOKE_SIZE", y = "BORE_OIL_VOL") +
  ggtitle("")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol = 4)

# Create a subset of petroleum with the selected variables
subset_petroleum <- petroleum[, c("BORE_OIL_VOL", "AVG_BOTTOMHOLE_PRESSURE", "AVG_BOTTOMHOLE_TEMPERATURE", 
                                  "AVG_DP_TUBING", "AVG_CHOKE_SIZE_P", "AVG_WHP_P", "AVG_WHT_P", "DP_CHOKE_SIZE")]

# Compute the correlation matrix
correlation_matrix <- cor(subset_petroleum)
correlation_matrix_rounded <- round(correlation_matrix, digits = 1)
# Plot the reordered correlation matrix with 1 decimal place labels and coefficients
library(corrplot)
corrplot(correlation_matrix_rounded, method = "color", 
         tl.col = "black", tl.srt = 45, diag = FALSE, sig.level = 0.01, insig = "blank",
         tl.cex = 0.8, order = "hclust", tl.offset = 0.5, 
         addCoef.col = "black", number.cex = 0.7, number.digits = 1)

#Plot AVG_DP_TUBING against other Independent Variables
plot8 <- ggplot(petroleum, aes(x = AVG_BOTTOMHOLE_PRESSURE, y = AVG_DP_TUBING)) +
  geom_point() +
  labs(x = "AVG_BOTTOMHOLE_PRESSURE", y = "AVG_DP_TUBING") +
  ggtitle("")
plot9 <- ggplot(petroleum, aes(x = AVG_WHP_P, y = AVG_DP_TUBING)) +
  geom_point() +
  labs(x = "AVG_WHP_P", y = "AVG_DP_TUBING") +
  ggtitle("")
plot10 <- ggplot(petroleum, aes(x = DP_CHOKE_SIZE, y = AVG_DP_TUBING)) +
  geom_point() +
  labs(x = "DP_CHOKE_SIZE", y = "AVG_DP_TUBING") +
  ggtitle("")
grid.arrange(plot8, plot9,plot10, ncol = 3)

#Plot DP_CHOKE_SIZE against other Independent Variables
plot11 <- ggplot(petroleum, aes(x = AVG_WHP_P, y = DP_CHOKE_SIZE)) +
  geom_point() +
  labs(x = "AVG_WHP_P", y = "DP_CHOKE_SIZE") +
  ggtitle("")
plot12 <- ggplot(petroleum, aes(x = AVG_CHOKE_SIZE_P, y = DP_CHOKE_SIZE)) +
  geom_point() +
  labs(x = "AVG_CHOKE_SIZE_P", y = "DP_CHOKE_SIZE") +
  ggtitle("")
plot13 <- ggplot(petroleum, aes(x = AVG_DP_TUBING, y = DP_CHOKE_SIZE)) +
  geom_point() +
  labs(x = "AVG_DP_TUBING", y = "DP_CHOKE_SIZE") +
  ggtitle("")
grid.arrange(plot11, plot12,plot13, ncol = 3)

# Only need 1 Y:
petroleum = petroleum[-c(1,11,12)]
#Delete highly correlated Xs:
petroleum$DP_CHOKE_SIZE = NULL
petroleum$AVG_DP_TUBING = NULL
#add Year
petroleum$DATEPRD <- as.Date(petroleum$DATEPRD, format = "%Y/%m/%d")

# turn date into year 
petroleum$Year <- year(petroleum$DATEPRD)
petroleum$Year <- as.integer(petroleum$Year)
petroleum = petroleum[-1]
# To facilitate comparison, standardize the explanatory variables.
petroleum$AVG_BOTTOMHOLE_PRESSURE <- scale(petroleum$AVG_BOTTOMHOLE_PRESSURE)
petroleum$AVG_BOTTOMHOLE_TEMPERATURE <- scale(petroleum$AVG_BOTTOMHOLE_TEMPERATURE)
petroleum$AVG_CHOKE_SIZE_P <- scale(petroleum$AVG_CHOKE_SIZE_P)
petroleum$AVG_WHP_P <- scale(petroleum$AVG_WHP_P)
petroleum$AVG_WHT_P <- scale(petroleum$AVG_WHT_P)
#Split:
set.seed(13)
train = sample.split(Y=petroleum$BORE_OIL_VOL, SplitRatio = 0.8)
trainset = petroleum[train == T,]
testset = petroleum[train == F,]

## Linear regression
# Importance Analysis
library(car)
library(ggplot2)
library(dplyr)
# create linear regression model as follows
m1<-lm(BORE_OIL_VOL ~ AVG_BOTTOMHOLE_PRESSURE+AVG_BOTTOMHOLE_TEMPERATURE+AVG_CHOKE_SIZE_P
       +AVG_WHP_P+AVG_WHT_P,data = trainset)
# check the regression result
summary(m1)
# use car package to calculate VIF of model 1
vif_value <- vif(m1) 
vif_value
# prediction
predictions_1 <- predict(m1, newdata = testset) 
# create a df including predicted y and actual y 
df1 <- data.frame(predictions_1, testset$BORE_OIL_VOL)
# create scatter plot showing predicted y vs actual y
ggplot(df1, aes(x = predictions_1, y = testset.BORE_OIL_VOL)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(x = "Predicted Y", y = "Actual Y", title = "Predicted vs. Actual Y") +
  theme_minimal()
# evaluate the prediction
rmse1 <- sqrt(mean((testset$BORE_OIL_VOL - predictions_1)^2))
mae1 <- mean(abs(testset$BORE_OIL_VOL - predictions_1))
# print performance metrics matrix
performance_metrics1<-data.frame(RMSE=rmse1,MAE=mae1)
performance_metrics1

# Prediction Analysis
# Introduce Year as another explanatory variable
# create linear regression model as follows
m2<-lm(BORE_OIL_VOL ~ AVG_BOTTOMHOLE_PRESSURE+AVG_BOTTOMHOLE_TEMPERATURE+AVG_CHOKE_SIZE_P
       +AVG_WHP_P+AVG_WHT_P+Year,data = trainset)
# check the regression result
summary(m2)
predictions_2 <- predict(m2, newdata = testset) 
# create a df including predicted y and actual y 
df2 <- data.frame(predictions_2, testset$BORE_OIL_VOL)
# create scatter plot showing predicted y vs actual y
ggplot(df2, aes(x = predictions_2, y = testset.BORE_OIL_VOL)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(x = "Predicted Y", y = "Actual Y", title = "Predicted vs. Actual Y") +
  theme_minimal()
# evaluate the prediction
rmse2 <- sqrt(mean((testset$BORE_OIL_VOL - predictions_2)^2))
mae2 <- mean(abs(testset$BORE_OIL_VOL - predictions_2))
# print performance metrics matrix
performance_metrics2<-data.frame(RMSE=rmse2,MAE=mae2)
performance_metrics2

# cross-validation 
# Specify the number of folds for cross-validation
num_folds <- 5
# Calculate the number of observations for each fold
fold_size <- nrow(petroleum) %/% num_folds
# Initialize variables to store performance metrics
performance_metrics3 <- data.frame(RMSE=rmse1,MAE=mae1)
# Perform cross-validation
for (fold in 1:num_folds-1) {
  # Calculate the index range for each fold
  start_index <- fold * fold_size + 1
  end_index <- (fold+1) * fold_size
  # Split the dataset into training and testing sets
  test_fold <- petroleum[start_index:end_index, ]
  train_fold <- petroleum[1:start_index, ]
  # use train_folds as training data to train the model
  # and then evaluate the model on test_fold
  m3<-lm(BORE_OIL_VOL ~ AVG_BOTTOMHOLE_PRESSURE+AVG_BOTTOMHOLE_TEMPERATURE+AVG_CHOKE_SIZE_P
         +AVG_WHP_P+AVG_WHT_P+Year,data = train_fold)
  # Evaluate the model performance and record it in performance metrics
  predictions_cv <- predict(m3, newdata = test_fold)
  actual_values <- test_fold$BORE_OIL_VOL
  rmse3 <- sqrt(mean((predictions_cv - actual_values)^2))
  mae3 <- mean(abs(predictions_cv - actual_values))
  performance_metrics3[fold, ] <- c(rmse3,mae3)
}
# print performance metrics matrix
performance_metrics3
# check the regression result
summary(m3)


##CART
#Importance Analysis
library(rpart)
library(rpart.plot)
set.seed(2023)   # for CV
cart1 <- rpart(BORE_OIL_VOL ~ AVG_BOTTOMHOLE_PRESSURE+AVG_BOTTOMHOLE_TEMPERATURE+AVG_CHOKE_SIZE_P
       +AVG_WHP_P+AVG_WHT_P, data = trainset, method = 'anova', cp = 0)
tail(printcp(cart1))      ## Turning Point not clear. But error reduces sig.
plotcp(cart1)
rpart.plot(cart1)

# Use my Rcode given in AAD1 Chap 8 or Unit 5 slides to get best tree automatically.
# Compute min CVerror + 1SE in maximal tree m2 
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + 
  cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree price.cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
# Get geometric mean of the identified min CP value and the CP above if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
# Get best tree based on 10 fold CV with 1 SE
cart2 <- prune(cart1, cp = cp.opt)
tail(printcp(cart2))
rpart.plot(cart2,cex = 0.1)
library(rpart.plot)
prp(cart2, cex = 0.15, extra = 1, branch = 0.5)
# importance anaylsis
cart2$variable.importance #!= gini
cart2.scaledVarImpt <- round(100*cart1$variable.importance/sum(cart1$variable.importance))
cart2.scaledVarImpt
cart2.rules = rpart.rules(cart2, nn = T, cover = T)
View(cart2.rules)
# use model to predict
predictions <- predict(cart2, newdata = testset)
# MSE
mse <- mean((testset$`BORE_OIL_VOL`- predictions)^2)
mse
# RMSE
rmse <- sqrt(mse)
rmse
#Mean Absolute Error, MAE
mae <- mean(abs(predictions - testset$`BORE_OIL_VOL`))
#R_squared
r_squared <- 1 - sum((testset$`BORE_OIL_VOL`- predictions)^2) / sum((testset$`BORE_OIL_VOL` - mean(testset$`BORE_OIL_VOL`))^2)
r_squared
# Scatter plot
plot(testset$`BORE_OIL_VOL`, predictions, main = "Scatter Plot of Actual vs. Predicted Oil volume", 
     xlab = "Actual Values", ylab = "Predicted Values",col = 'green')
abline(a = 0, b = 1, col = "blue")

#Prediction Analysis
#Add Year as a variable
set.seed(2023)   # for CV
cart1 <- rpart(BORE_OIL_VOL ~ AVG_BOTTOMHOLE_PRESSURE+AVG_BOTTOMHOLE_TEMPERATURE+AVG_CHOKE_SIZE_P
         +AVG_WHP_P+AVG_WHT_P+Year, data = trainset, method = 'anova', cp = 0)
printcp(cart1)            ## Too many trees. Only 200 shown.
tail(printcp(cart1))      ## Turning Point not clear. But error reduces sig.
plotcp(cart1)
rpart.plot(cart1)
# Use my Rcode given in AAD1 Chap 8 or Unit 5 slides to get best tree automatically.
# Compute min CVerror + 1SE in maximal tree m2 
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + 
  cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree price.cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
# Get geometric mean of the identified min CP value and the CP above if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
# Get best tree based on 10 fold CV with 1 SE
cart2 <- prune(cart1, cp = cp.opt)
#tail(printcp(cart2))
rpart.plot(cart2,cex = 0.5)
cart2$variable.importance #!= gini
cart2.scaledVarImpt <- round(100*cart1$variable.importance/sum(cart1$variable.importance))
cart2.scaledVarImpt
cart2.rules = rpart.rules(cart2, nn = T, cover = T)
View(cart2.rules)
predictions <- predict(cart2, newdata = testset)
mse <- mean((testset$`BORE_OIL_VOL`- predictions)^2)
rmse <- sqrt(mse)
rmse
mae <- mean(abs(predictions - testset$`BORE_OIL_VOL`))
mae
r_squared <- 1 - sum((testset$`BORE_OIL_VOL`- predictions)^2) / sum((testset$`BORE_OIL_VOL` - mean(testset$`BORE_OIL_VOL`))^2)
r_squared
plot(testset$`BORE_OIL_VOL`, predictions, main = "Scatter Plot of Actual vs. Predicted Oil volume", 
     xlab = "Actual Values", ylab = "Predicted Values",col = 'green')
abline(a = 0, b = 1, col = "blue")


#Time-cross Validation with year
num_folds <- 5
fold_size <- nrow(petroleum) %/% num_folds
performance_metrics <- data.frame(RMSE = numeric(num_folds-1), MAE = numeric(num_folds-1))
for (fold in 1:(num_folds-1)) {
  start_index <- fold * fold_size + 1
  end_index <- (fold+1) * fold_size 
  test_fold <- petroleum[start_index:end_index, ]
  train_folds <- petroleum[1:start_index, ]

  cart <- rpart(`BORE_OIL_VOL` ~ ., data = train_folds, method = 'anova', cp = 0)
  
  # Use my Rcode given in AAD1 Chap 8 or Unit 5 slides to get best tree automatically.
  # Compute min CVerror + 1SE in maximal tree m2 
  CVerror.cap <- cart$cptable[which.min(cart$cptable[,"xerror"]), "xerror"] + 
    cart$cptable[which.min(cart$cptable[,"xerror"]), "xstd"]
  i <- 1; j<- 4
  while (cart$cptable[i,j] > CVerror.cap) {
    i <- i + 1
  }
  cp.opt = ifelse(i > 1, sqrt(cart$cptable[i,1] * cart$cptable[i-1,1]), 1)
  # Get best tree based on 10 fold CV with 1 SE
  cart.cut <- prune(cart, cp = cp.opt)

  predicted_values <- predict(cart.cut, newdata = test_fold)
  actual_values <- test_fold$`BORE_OIL_VOL`
  rmse <- sqrt(mean((predicted_values - actual_values)^2,na.rm = TRUE))
  mae <- mean(abs(predicted_values - actual_values),na.rm = TRUE)
  r2 <- 1 - sum((actual_values- predicted_values)^2,na.rm = TRUE) / sum((actual_values - mean(actual_values))^2,na.rm = TRUE)
  performance_metrics[fold, ] <- c(rmse, mae,r2)
  plot(test_fold$`BORE_OIL_VOL`, predicted_values, main = "Scatter Plot of Actual vs. Predicted Oil volume", 
                 xlab = "Actual Values", ylab = "Predicted Values",col = 'green')

}
print(performance_metrics)


### Random Forest
# Model without year
library(data.table)
library(randomForest)
library(caTools)
library(dplyr)
library(nlme)
set.seed(13)
RF_train = randomForest(BORE_OIL_VOL ~ .-Year , data=trainset)
varImpPlot(RF_train)
predictions_3 = predict(RF_train, newdata = testset)
RMSE_RF = round(sqrt(mean((testset$BORE_OIL_VOL - predictions_3)^2)))
RMSE_RF #160
MAE_RF <- mean(abs(predictions_3 - testset$BORE_OIL_VOL))
MAE_RF #66.32864
r_squared_RF = 1 - sum((testset$`BORE_OIL_VOL`- predictions_3)^2) / sum((testset$`BORE_OIL_VOL` - mean(testset$`BORE_OIL_VOL`))^2)
r_squared_RF #0.9759692

# Cross-validation of non-year
num_folds = 5
fold_size = nrow(petroleum) %/% num_folds #462
performance_metrics <- data.frame(RMSE = numeric(num_folds), MAE = numeric(num_folds),
                                  R_squared = numeric(num_folds))
for (fold in 1:(num_folds - 1)) {
  start_index <- fold * fold_size + 1
  end_index <- (fold + 1) * fold_size
  test_fold <- petroleum[start_index:end_index, ]
  train_folds <- petroleum[1:start_index, ]
  cart1 <- rpart(BORE_OIL_VOL ~ .-Year, data = train_folds, method = 'anova', cp = 0)
  predicted_values <- predict(cart1, newdata = test_fold)
  actual_values <- test_fold$`BORE_OIL_VOL`
  rmse <- sqrt(mean((predicted_values - actual_values)^2))
  mae <- mean(abs(predicted_values - actual_values))
  R_squared = 1 - sum((actual_values- predicted_values)^2) / sum((actual_values - mean(actual_values))^2)
  performance_metrics[fold, ] <- c(rmse, mae, R_squared)
}
print(performance_metrics) #102,89,-3

# Model with year
RF_train = randomForest(BORE_OIL_VOL ~ ., data=trainset)
varImpPlot(RF_train)
predictions_3 = predict(RF_train, newdata = testset)
RMSE_RF = round(sqrt(mean((testset$BORE_OIL_VOL - predictions_3)^2)))
RMSE_RF #174
MAE_RF <- mean(abs(predictions_3 - testset$BORE_OIL_VOL))
MAE_RF #57.48579
r_squared_RF = 1 - sum((testset$`BORE_OIL_VOL`- predictions_3)^2) / sum((testset$`BORE_OIL_VOL` - mean(testset$`BORE_OIL_VOL`))^2)
r_squared_RF #0.9714675

# Cross-validation with year
num_folds = 5
fold_size = nrow(petroleum) %/% num_folds #462
performance_metrics <- data.frame(RMSE = numeric(num_folds), MAE = numeric(num_folds),
                                  R_squared = numeric(num_folds))
for (fold in 1:(num_folds - 1)) {
  start_index <- fold * fold_size + 1
  end_index <- (fold + 1) * fold_size
  test_fold <- petroleum[start_index:end_index, ]
  train_folds <- petroleum[1:start_index, ]
  cart1 <- rpart(BORE_OIL_VOL ~ ., data = train_folds, method = 'anova', cp = 0)
  predicted_values <- predict(cart1, newdata = test_fold)
  actual_values <- test_fold$`BORE_OIL_VOL`
  rmse <- sqrt(mean((predicted_values - actual_values)^2))
  mae <- mean(abs(predicted_values - actual_values))
  R_squared = 1 - sum((actual_values- predicted_values)^2) / sum((actual_values - mean(actual_values))^2)
  performance_metrics[fold, ] <- c(rmse, mae, R_squared)
}
print(performance_metrics) #108,95,-3

plot(predictions_3, testset$BORE_OIL_VOL, col = "orange", 
     xlab = "Predicted", ylab = "Actual", main = "Predicted vs. Actual")
abline(lm(testset$BORE_OIL_VOL~predictions_3))

