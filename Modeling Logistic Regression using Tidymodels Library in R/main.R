# Title     : TODO
# Objective : TODO
# Created by: Rahul-Raoniar
# Created on: 27-04-2021
library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(tidymodels)

# load the diabetes dataset
data(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)


Diabetes <- na.omit(PimaIndiansDiabetes2)

glimpse(Diabetes)

levels(Diabetes$diabetes)

Diabetes$diabetes <- relevel(Diabetes$diabetes, ref = "pos")



# Create data split for train and test
set.seed(123)
diabetes_split <- initial_split(Diabetes,
                               prop = 0.75,
                               strata = diabetes)

# Create training split
diabetes_train <- diabetes_split %>%
  training()

# Create the test data
diabetes_test <- diabetes_split %>%
  testing()

# Check the number of rows
nrow(diabetes_train)
nrow(diabetes_test)



##################################
# Fitting a logistic regression model

fitted_logistic_model <- logistic_reg() %>%
  # Set the engine
  set_engine("glm") %>%
  # Set the mode
  set_mode("classification") %>%
  fit(diabetes~.,
      data = diabetes_train)

tidy(fitted_logistic_model)

tidy(fitted_logistic_model, exponentiate = TRUE)

tidy(fitted_logistic_model, exponentiate = TRUE) %>%
  filter(p.value < 0.05)


# Class prediction
pred_class <- predict(fitted_logistic_model,
                      new_data = diabetes_test,
                      type = "class")

pred_class[1:5,]

# Prediction Probabilities
pred_proba <- predict(fitted_logistic_model,
                      new_data = diabetes_test,
                      type = "prob")

pred_proba[1:5,]


# Combining class and probabilities with test dataset
diabetes_results <- diabetes_test %>%
  select(diabetes) %>%
  bind_cols(pred_class, pred_proba)

diabetes_results[1:5, ]

# Model evaluation

# Confusuin Matrix
conf_mat(diabetes_results, truth = diabetes,
    estimate = .pred_class)

accuracy(diabetes_results, truth = diabetes,
    estimate = .pred_class)

sens(diabetes_results, truth = diabetes,
    estimate = .pred_class)

spec(diabetes_results, truth = diabetes,
    estimate = .pred_class)

precision(diabetes_results, truth = diabetes,
    estimate = .pred_class)

recall(diabetes_results, truth = diabetes,
      estimate = .pred_class)

f_meas(diabetes_results, truth = diabetes,
    estimate = .pred_class)

kap(diabetes_results, truth = diabetes,
    estimate = .pred_class)

mcc(diabetes_results, truth = diabetes,
    estimate = .pred_class)

## Custom matrics
custom_metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas, kap, mcc)

custom_metrics(diabetes_results,
               truth = diabetes,
               estimate = .pred_class)

roc_auc(diabetes_results,
       truth = diabetes,
       .pred_pos)

diabetes_results %>%
  roc_curve(truth = diabetes, .pred_pos) %>%
  autoplot()

library(caret)
confusionMatrix(diabetes_results$.pred_class, diabetes_results$diabetes, positive="pos")