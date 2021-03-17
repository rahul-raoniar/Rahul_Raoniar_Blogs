###############################################
## Diabetes prediction using classification tree
###############################################


# load Libraries

library(mlbench)    #diabetes dataset
library(rpart)      # Decision tree training
library(rpart.plot) #tree plotting
library(caret)      #Confusion Matrix
library(Metrics)    #Accuracy metric

# load the diabetes dataset
data(PimaIndiansDiabetes2)

##########################
# Details of the dataset
##########################

# pregnant:	 Number of times pregnant
#  glucose:	 Plasma glucose concentration (glucose tolerance test)
# pressure:	 Diastolic blood pressure (mm Hg)
#  triceps:	 Triceps skin fold thickness (mm)
#  insulin:	 2-Hour serum insulin (mu U/ml)
#     mass:	 Body mass index (weight in kg/(height in m)\^2)
# pedigree:	 Diabetes pedigree function
#      age:  Age (years)
# diabetes:	 Class variable (test for diabetes)

# Save data to Diabetes
Diabetes_raw <- na.omit(PimaIndiansDiabetes2) # Raw data


Diabetes <- na.omit(PimaIndiansDiabetes2) # Data for modeling

dplyr::glimpse(Diabetes)




#############################################
# Splitting to train (80%) and test (20%)
#############################################

set.seed(123)

index <- sample(2, nrow(Diabetes), prob = c(0.8, 0.2), replace = TRUE)

Diabetes_train <- Diabetes[index==1, ] # Train data
Diabetes_test <- Diabetes[index == 2, ] # Test data


names(Diabetes_train)


print(dim(Diabetes_train))
print(dim(Diabetes_test))



##########################################
# Training a tree model using rpart library
##########################################


# Train a decision tree model
Diabetes_model <- rpart(formula = diabetes ~., 
                        data = Diabetes_train, 
                        method = "class")


##########################################
# Plotting the trained model
##########################################

rpart.plot(x = Diabetes_model, yesno =2, type = 0, extra = 0)



#########################################
## Model performance evaluation on test dataset
#########################################

# class prediction
class_predicted <- predict(object = Diabetes_model,  
                            newdata = Diabetes_test,   
                            type = "class")  

# Generate a confusion matrix for the test data

confusionMatrix(data = class_predicted,       
                reference = Diabetes_test$diabetes)

# Model accuracy on test data

accuracy(actual = class_predicted,       
         predicted = Diabetes_test$diabetes)


###########################################
## Tree splitting criteria based comparision
###########################################


# Model training based on gini-based splitting criteria
Diabetes_model1 <- rpart(formula = diabetes ~ ., 
                         data = Diabetes_train, 
                         method = "class",
                         parms = list(split = "gini"))

# Model training based on information gain-based splitting criteria
Diabetes_model2 <- rpart(formula = diabetes ~ ., 
                         data = Diabetes_train, 
                         method = "class",
                         parms = list(split = "information"))

# Generate class predictions on the test data using gini-based splitting criteria
pred1 <- predict(object = Diabetes_model1, 
                 newdata = Diabetes_test,
                 type = "class")    

# Generate class predictions on test data using information gain based splitting criteria
pred2 <- predict(object = Diabetes_model2, 
                 newdata = Diabetes_test,
                 type = "class")



# Compare classification accuracy on test data
accuracy(actual = Diabetes_test$diabetes, 
   predicted = pred1)

accuracy(actual = Diabetes_test$diabetes, 
   predicted = pred2)  


rpart.plot(x = Diabetes_model1, yesno = 2, type = 0, extra = 0)


#########################################
## Tree Pruning of Diabetes_model1
#########################################

# Plotting Cost Parameter (CP) Table
plotcp(Diabetes_model1)

# Plotting the Cost Parameter (CP) Table
print(Diabetes_model1$cptable)


# Retrieve of optimal cp value based on cross-validated error
index <- which.min(Diabetes_model1$cptable[, "xerror"])

cp_optimal <- Diabetes_model1$cptable[index, "CP"]


# Pruning tree based on optimal CP value

Diabetes_model1_opt <- prune(tree = Diabetes_model1, cp = cp_optimal)


# Plot the optimized model

rpart.plot(x = Diabetes_model1_opt, yesno = 2, type = 0, extra = 0)

pred3 <- predict(object = Diabetes_model1_opt, 
                 newdata = Diabetes_test,
                 type = "class")


accuracy(actual = Diabetes_test$diabetes, 
         predicted = pred3) 


#############################
## Hyper parameter Grid Search
#############################

# Setting values for minsplit and maxdepth

## the minimum number of observations that must exist in a node in order for a split to be attempted.
## Set the maximum depth of any node of the final tree
minsplit <- seq(1, 20, 1)
maxdepth <- seq(1, 20, 1)


# Generate a search grid 
hyperparam_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)



# Number of potential models in the grid
num_models <- nrow(hyperparam_grid)

# Create an empty list 
diabetes_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {
  
  minsplit <- hyperparam_grid$minsplit[i]
  maxdepth <- hyperparam_grid$maxdepth[i]
  
  # Train a model and store in the list
  diabetes_models[[i]] <- rpart(formula = diabetes ~ ., 
                             data = Diabetes_train, 
                             method = "class",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}

# Let's check model 50
diabetes_models[[50]]
rpart.plot(x = diabetes_models[[50]], yesno = 2, type = 0, extra = 0)





# Number of models inside the grid
num_models <- length(diabetes_models)

# Create an empty vector to store accuracy values
accuracy_values <- c()

# Use for loop for models accuracy estimation
for (i in 1:num_models) {
  
  # Retrieve the model i from the list
  model <- diabetes_models[[i]]
  
  # Generate predictions on test data 
  pred <- predict(object = model,
                  newdata = Diabetes_test,
                  type = "class")
  
  # Compute test accuracy and add to the empty vector accuracy_values 
  accuracy_values[i] <- accuracy(actual = Diabetes_test$diabetes, 
                         predicted = pred)
}

# Identify the model with maximum accuracy
best_model <- diabetes_models[[which.max(accuracy_values)]]


# Print the model hyper-parameters of the best model
best_model$control

# Best_model accuracy on test data
pred <- predict(object = best_model,
                newdata = Diabetes_test,
                type = "class")
accuracy(actual = Diabetes_test$diabetes, 
     predicted = pred)


rpart.plot(x = best_model, yesno = 2, type = 0, extra = 0)

