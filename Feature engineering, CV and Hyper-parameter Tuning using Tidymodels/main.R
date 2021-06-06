# Title     : TODO
# Objective : TODO
# Created by: Rahul-Raoniar
# Created on: 27-04-2021
library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(tidymodels)  # for fitting model

# load the diabetes dataset
data(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)

# Removing NA values
Diabetes <- na.omit(PimaIndiansDiabetes2)

glimpse(Diabetes)

# Setting reference level
levels(Diabetes$diabetes)

Diabetes$diabetes <- relevel(Diabetes$diabetes, ref = "pos")


########################################
# Creating a categorical column for demonstration

range(Diabetes$pregnant)

Diabetes <- Diabetes %>%
  mutate(pregnant = case_when(
    between(pregnant, 0, 2)~"0-2",
    between(pregnant, 2, 4)~"2-4",
    between(pregnant, 4, 20)~">4")) %>%

  mutate(pregnant = factor(pregnant, levels = c("0-2", "2-4", ">4")))


levels(Diabetes$pregnant)
table(Diabetes$pregnant)

names(Diabetes)



# Create data split for train and test
set.seed(123)
diabetes_split <- initial_split(Diabetes,
                               prop = 0.75,
                               strata = diabetes)

# Create train data
diabetes_train <- diabetes_split %>%
  training()

# Create test data
diabetes_test <- diabetes_split %>%
  testing()

# Check the number of rows
nrow(diabetes_train)
nrow(diabetes_test)


####################################
# Creating model pipe line
dt_model <- decision_tree() %>%
  # Specify the engine
  set_engine("rpart") %>%
  # Specify the mode
  set_mode("classification")

# Check for correlation
diabetes_train %>%
  # Select numeric columns
  select_if(is.numeric) %>%
  # Calculate correlation matrix
  cor()

# Plot correlated predictors
ggplot(diabetes_train, aes(x = insulin, y = glucose)) +
  # Add points
  geom_point()  +
  # Add title
  labs(title = "Insulin vs. Glucose",
       y = 'Glucose', x = 'Insulin')

# Creating a feature engineering pipeline
diabetes_recipe <- recipe(diabetes~.,
                        data = diabetes_train) %>%
  # Correlation filter
  step_corr(all_numeric(), threshold = 0.4) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>%
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())


# Apply to training data
diabetes_recipe_prep <- diabetes_recipe %>%
  prep(training = diabetes_train)

# Transform training data
diabetes_training_prep <- diabetes_recipe_prep %>%
  bake(new_data = NULL)

head(diabetes_training_prep)

# Apply to test data
diabetes_test_prep <- diabetes_recipe_prep %>%
  bake(new_data = diabetes_test)

head(diabetes_test_prep)


# Train DT model
diabetes_DT_fit <- dt_model %>%
  fit(diabetes ~ ., data = diabetes_training_prep)

# Obtain class predictions
class_preds <- predict(diabetes_DT_fit,
                       new_data = diabetes_test_prep,
                       type = 'class')

class_preds[1:5, ]

# Obtain estimated probabilities
pred_proba <- predict(diabetes_DT_fit,
                       new_data = diabetes_test_prep,
                       type = 'prob')
pred_proba[1:5, ]


# Combine test set results
diabetes_results <- diabetes_test_prep %>%
  bind_cols(class_preds, pred_proba)

diabetes_results %>%
  head()

custom_metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas, kap, mcc)
custom_metrics(diabetes_results,
               truth = diabetes,
               estimate = .pred_class)




############################################
# CV

# Generating a workflow
diabetes_DT_wkfl <- workflow() %>%
  # Add model
  add_model(dt_model) %>%
  # Add recipe
  add_recipe(diabetes_recipe)

# Create cross validation folds
set.seed(123)
diabetes_folds <- vfold_cv(diabetes_train, v = 10,
                       strata = diabetes)

# Create custom metrics function
diabetes_metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas, kap, mcc)

# Fit resamples
diabetes_dt_resampling <- diabetes_DT_wkfl %>%
  fit_resamples(resamples = diabetes_folds,
                metrics = diabetes_metrics)

# View performance metrics
diabetes_dt_resampling %>%
  collect_metrics()

# Detailed cross validation results
dt_rs_results <- diabetes_DT_resampling %>%
  collect_metrics(summarize = FALSE) %>%
  group_by(.metric) %>%
  summarize(min = min(.estimate),
            median = median(.estimate),
            max = max(.estimate))
dt_rs_results


####################################
# Hyper-parameter Tuning

# Set hyperparameters to tune
tuned_dt_model <- decision_tree(tree_depth = tune(),
                                cost_complexity = tune(),
                                min_n = tune()) %>%
  # Specify engine
  set_engine('rpart') %>%
  # Specify mode
  set_mode('classification')

# Updating the workflow
diabetes_tune_wkfl <- diabetes_DT_wkfl %>%
  # Replace model
  update_model(tuned_dt_model)

diabetes_tune_wkfl


# generating a random grid for model tuning

set.seed(122)
dt_random_grid <- grid_random(parameters(tuned_dt_model),
               size = 10)

dt_random_grid

# Hyperparameter tuning
dt_model_tuning <- diabetes_tune_wkfl %>%
  tune_grid(resamples = diabetes_folds,
            grid = dt_random_grid,
            metrics = diabetes_metrics)

# View results
dt_model_tuning %>%
  collect_metrics()

# View results
dt_model_tuning %>%
  collect_metrics(summarize = FALSE)


dt_model_tuning %>%
  collect_metrics(summarize = FALSE) %>%
  filter(.metric=="mcc") %>%
  group_by(id) %>%
  summarize(min_mcc = min(.estimate),
            median_mcc = median(.estimate),
            max_mcc = max(.estimate))


# Display top five models
dt_model_tuning %>%
  show_best(metric = "mcc", n = 5)

# Taking the best model and refitting

best_dt_model <- dt_model_tuning %>%
  select_best(metric = "mcc")

best_dt_model

# Finalize work flow
diabetes_final_fit <- diabetes_tune_wkfl %>%
  finalize_workflow(best_dt_model)

diabetes_final_fit %>%
  last_fit(split = diabetes_split,
           metrics = diabetes_metrics) %>%
  collect_metrics()


diabetes_tune_wkfl %>%
  finalize_workflow(best_dt_model) %>%
  last_fit(split = diabetes_split) %>%
  collect_predictions() %>%
  roc_auc(truth = diabetes,
       .pred_pos)


# Generating an ROC curve
diabetes_tune_wkfl %>%
  finalize_workflow(best_dt_model) %>%
  last_fit(split = diabetes_split) %>%
  collect_predictions() %>%
  roc_curve(truth = diabetes, .pred_pos) %>%
  autoplot()