# sys6018-competition-titanic
# Rohan Bapat - rb2te 
SYS 6018 Kaggle Titanic submission
The code is an approach to predict survival in the Titanic dataset using logistic regression

Approach - 

1. Read train dataset
2. Clean the train dataset
3. Cross-validation  - Split the train dataset further into test and train datasets
4. Create a logistic regression based model on the train dataset. Tune the parameters and select the model which gives lowest
   AIC value
5. Use this model to predict survival for the actual test dataset

