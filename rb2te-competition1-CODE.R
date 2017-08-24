
# Import tidyverse package
library(tidyverse)

# Set working directory to required folder
#setwd('C:\\Users\\Rohan Bapat\\Documents\\Classes\\SYS 6018\\Assignment - Titanic')

# Set seed to ensure repeatablity
set.seed(123)

# Read the train dataset first
# We will build the classifier model using the train dataset. Later we will use this model on the
# test dataset to get the survival predictions 
titanic_train <- read_csv('train.csv')

# ---------- Clean input data -------------------------------------

# Eliminate columns - PassengerId, Name and Cabin, as they won't be used to predict Survival
# PassengerId eliminated since it's a unique identifier
# Name eliminated since it's a unique identifier
# Cabin eliminated since it's not simple to parse it into meaningful components
x_data <- subset(titanic_train, select = -c(PassengerId, Name, Ticket, Cabin))

# Impute missing values in Age with the median age value
x_data[is.na(x_data['Age']),'Age'] <- median(x_data$Age, na.rm = T)

# Remove any other missing values
x_data <- x_data[!is.na(x_data),]

# ---------- Cross validation datasets ------------------------------

# Specify test train split ratio
test_train_split <- 0.7  # The train dataset is further split in the ratio train:test::70%:30%

# Create vector of random index to split test and train
sample_size <- sample(1:nrow(x_data),round(nrow(x_data)*test_train_split))

# Split x_data into train and test datasets
x_train <- x_data[sample_size,]
x_test <- x_data[-sample_size,]

#----------- Build logistic models for classification ---------------

# Create logistic regression model using all variables and output summary
log_model1 <- glm(Survived~., data = x_train, family = 'binomial')
summary(log_model1)

# Remove variables with high P value and create summary for this logistic model
log_model2 <- glm(Survived~.-Parch-Fare, data = x_train, family = 'binomial')
summary(log_model2)

# The AIC value for log_model2 is lower than AIC for log_model1. Hence we will proceed with log_model2

# ----------- Predict on test dataset -------------------------------

# Perform cross-validation on test data to determine accuracy. Use log_model2 for higher prediction accuracy
cv_predict_survival <- predict(log_model2, newdata = x_test, type = "response")

# Create zero vector 
cv_probs <- rep(0,length(cv_predict_survival))

# Encode logistic prediction - prediction > 0.5 coded as 1, <=0.5 coded as 0
cv_probs[cv_predict_survival>0.5]<-1

# ------------ Calculate prediction accuracy ------------------------

# Create prediction accuracy matrix
cv_accuracy_table <- table(cv_probs,x_test$Survived)

# Convert matrix into dataframe
cv_accuracy_df <- as.data.frame.matrix(cv_accuracy_table)

# Calculate and print accuracy
cv_accuracy <- (cv_accuracy_df['0','0'] + cv_accuracy_df['1','1'])/sum(cv_accuracy_df)
print(cv_accuracy)

# Build logistic model by using both test and train datasets of the original train dataset
log_model_final <- glm(Survived~.-Parch-Fare, data = x_data, family = "binomial")

# ------------ Predict on actual test dataset ----------------------

# Import test dataset to make predictions
titanic_test <- read_csv('test.csv')

# ------------ Clean test dataset ----------------------------------

# Eliminate columns - PassengarId, Name and Cabin, as they won't be used to predict Survival
y_data <- subset(titanic_test, select = -c(PassengerId, Name, Ticket, Cabin))

# Impute missing values in Age with the median age value
y_data[is.na(y_data['Age']),'Age'] <- median(y_data$Age, na.rm = T)
y_data[is.na(y_data['Fare']),'Fare'] <- median(y_data$Fare, na.rm = T)

# ------------ Predict survival using logistic model ---------------

y_survival <- predict(log_model_final, newdata = y_data, type = "response")

# Create zero vector 
y_probs <- rep(0,length(y_survival))

# Encode logistic prediction - prediction > 0.5 coded as 1, <=0.5 coded as 0
y_probs[y_survival>0.5]<-1

# ------------ Output prediction to csv ----------------------------

outfile <- cbind(titanic_test['PassengerId'],y_probs)

# Add specified column names
colnames(outfile) <- c("PassengerId","Survived")

# Output predictions to csv
write.csv(outfile, file = "rb2te_titanic_predictions.csv", row.names = F)
