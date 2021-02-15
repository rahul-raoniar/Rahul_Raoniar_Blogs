# Article 1
# Shrikant I. Bangdiwala (2018): Regression: binary logistic, International Journal
# of Injury Control and Safety Promotion, DOI: 10.1080/17457300.2018.1486503

# Article 2
# Leeper, T.J., 2017. Interpreting regression results using average marginal 
# effects with R's margins. Tech. rep. URL https://cran. r-project.
# org/web/packages/margins/index. html.


#############################
# Part 1: load Libraries
#############################
library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(tidyverse)   # or specifically you can use ggplot2 library
                     # for plotting
library(broom)       # Make model summary tidy
library(caret)       # use to compute confusion matrix
library(visreg)      # For potting logodds and probabilitis
library(margins)     # Use to calculate Average Marginal Effects
library(rcompanion)  # Use to calculate pseudo R2
library(ROCR)        # Use to calculate Reciever Opering Curve



#############################
# Part 2: Gather and clean data
#############################

data(package = "mlbench")



# load the diabetes dataset

data(PimaIndiansDiabetes2)


##########################
# Variables of the dataset
##########################

# I1: pregnant:	 Number of times pregnant
# I2:  glucose:	 Plasma glucose concentration (glucose tolerance test)
# I3: pressure:	 Diastolic blood pressure (mm Hg)
# I4:  triceps:	 Triceps skin fold thickness (mm)
# I5:  insulin:	 2-Hour serum insulin (mu U/ml)
# I6:     mass:	 Body mass index (weight in kg/(height in m)\^2)
# I7: pedigree:	 Diabetes pedigree function
# I8:      age:	 Age (years)

# D1: diabetes:	 Class variable (test for diabetes)


head(PimaIndiansDiabetes2)

glimpse(PimaIndiansDiabetes2)



# Save data to Diabetes

Diabetes <- na.omit(PimaIndiansDiabetes2) # dataset for modeling

head(Diabetes)

glimpse(Diabetes)




# Changing levels neg = 0 and pos = 1

levels(Diabetes$diabetes)

levels(Diabetes$diabetes) <- 0:1

levels(Diabetes$diabetes)

glimpse(Diabetes)

###############################################
# Part 3: Dividing randomly data samples into train and test dataset
###############################################


# Total number of rows in the credit data frame
n <- nrow(Diabetes)

# Number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n) 

# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)

# Subset the credit data frame to training indices only
train <- Diabetes[train_indices, ]  

# Exclude the training indices to create the test set
test <- Diabetes[-train_indices, ]  

paste("train sample size: ", dim(train)[1])
paste("test sample size: ", dim(test)[1])

####################################
# Part 4: Fitting a logistic regression model
####################################


model_logi <- glm(diabetes~., data = train, family = "binomial")

summary(model_logi) # see summary statistics


# Make data tidy using broom package
tidy(model_logi)

glance(model_logi) # Check model fitting

augment(model_logi) # obtain fitted values



#####################################
# Part 5: Calculating important statistics
#####################################

# Part 5 (A): Calculating the odd ratio

(exp(coef(model_logi)))

tidy(model_logi, exponentiate = TRUE, conf.level = 0.95) # odd ratio


# Part 5 (B): Logodds and probability plots


# Import and use visreg library

# Logodds of diabetes wrt to glucose level
visreg(model_logi, "glucose", xlab="Glucose level",
       ylab="Log odds (diabetes)")

# Logodds of diabetes wrt to pedigree level
visreg(model_logi, "pedigree", xlab="pedigree level",
       ylab="Log odds (diabetes)")




# Probabilities of diabetes wrt glucose
visreg(model_logi, "glucose", scale="response", rug=2, xlab="Glucose level",
       ylab="P(diabetes)")

# Probabilities of diabetes wrt pedigree
visreg(model_logi, "pedigree", scale="response", rug=2, xlab="pedigree level",
       ylab="P(diabetes)")




# Part 5 (C): Calculate marginal effect


######################################################

# While the estimated coefficients from logistic regression 
# are not easily interpretable

# 1. Log odds: represents the change in the log of odds of 
# outcome for a given change in a predictor  

# 2. odds ratios: might provide a better summary of the effects of 
# predictor on outcome variable (odds ratios are derived from
# exponentiation of the estimated coefficients from logistic
# regression). The Calculation and Interpretation of 
# Odds Ratios may be somewhat more meaningful.

# Marginal effects: Marginal effects are an alternative metric 
# that can be used to describe the impact of a preditor on 
# outcome variable. Marginal effects can be
# described as the change in outcome as a
# function of the change in the treatment 
# (or independent variable of interest) holding all other
# variables in the model constant. In linear regression,
# the estimated regression coefficients are marginal effects
# and are more easily interpreted (more on this later).


# There are two way of computing Marginal Effects

# a) Marginal Effect at Mean
# b) Average Marginal Effect 

# The magnitude of the marginal effect depends on the
# values of the other variables and their coefficients.

# The Marginal Effect at the Mean (MEM) is popular (i.e. compute the marginal
# effects when all x's are at their mean) but many think that 
# Average Marginal Effects (AMEs) are superior 


# Use "margins" library for Average Marginal Effect compulation


# Calculate average marginal effect
effects_logit_dia = margins(model_logi)



print(effects_logit_dia)


# Summary of marginal effect
summary(effects_logit_dia)



# Plot marginal effect
plot(effects_logit_dia)



# Plot marginal effect using ggplot2 library

effects_logit_diab = summary(effects_logit_dia)


ggplot(data = effects_logit_diab) +
  geom_point(mapping = aes(x = factor, y = AME)) +
  geom_errorbar(mapping = aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


#######################
# Part 6: Model Evaluation
#######################

# Part 6 (A): Misclassification identification 
#             using confusion matrix

pred <- predict(model_logi, test, type="response") # predict using test data

head(pred)

predicted <- round(pred) # round of the value; >0.5 will convert to 1 
      
head(predicted)          # else 0

# Side by side comparision

head(data.frame(observed = test$diabetes, predicted = predicted))


# Let's create a contigency table

tab <- table(Predicted = predicted, Reference = test$diabetes)

tab


sum(diag(tab))/sum(tab)*100


# Confusion matrix using caret package

confusionMatrix(tab)



#######
# Precision, Recall and F1 Score

library(yardstick)


act_pred <- data.frame(observed = test$diabetes, predicted = factor(predicted))
str(act_pred)

accuracy_est <- accuracy(act_pred, observed, predicted)
prec <- precision(act_pred, observed, predicted)
rec <- recall(act_pred, observed, predicted)
F1_score <- f_meas(act_pred, observed, predicted)

print(accuracy_est)
print(prec)
print(rec)
print(F1_score)


# Part 6 (B):
# Pseudo R2 and loglikelyhood ratio test

# Import and use rcompanion library


nagelkerke(model_logi)


# Part 6 (C)
# Compute the cutoff values
# Use ROCR Package

# Use the prediction function to generate a prediction result:


pred.rocr <- prediction(pred, test$diabetes)

eval <- performance(pred.rocr, "acc")

plot(eval)



# Identify best value (Cutoff vs Accuracy)

max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max] #y.values are accuracy measures
cut <- slot(eval, "x.values")[[1]][max] # x.values are cutoff measures


print(c(Accuracy = acc, Cutoff = cut))


# Part 6 (D)
# Receiver Operating Characteristic Curve computation
# Import ROCR library

# ROC (Receiver Operating Characteristic) Curve tells us about how good
# the model can distinguish between two things

# Use the performance function to obtain the performance measurement:

perf_rocr <- performance(pred.rocr, measure = "auc",
                         x.measure = "cutoff")

perf_rocr@y.values[[1]] <- round(perf_rocr@y.values[[1]], digits = 4)

perf.tpr.fpr.rocr <- performance(pred.rocr, "tpr", "fpr")

# pos (actual) --and-- predicted (pos) ->correctly identified -> True pos
# neg (actual) --and-- predicted (pos) ->Incorrectly identified -> False Pos
# pos (actual) --and-- predicted (not pos) ->Incorrectly rejected -> False neg
# neg (actual) --and-- predicted (not pos) ->Correctly rejected -> True neg


# Visualize ROC curve using plot function

plot(perf.tpr.fpr.rocr, colorize=T, 
     main = paste("AUC:", (perf_rocr@y.values)))
abline(a = 0, b = 1)
