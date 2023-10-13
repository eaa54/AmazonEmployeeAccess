library(tidymodels)
library(vroom)
library(tidyverse)
library(embed)

amazon <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/train.csv")
testData <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/test.csv")
view(amazon)
amazon <- amazon %>%
  mutate(ACTION = as.factor(ACTION)) #make response a factor

Amazon_recipe <- recipe(ACTION ~ ., amazon) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_other(all_nominal_predictors(), threshold = 0.001) %>% #takes predictor columns and makes an other col for values that occur less than 1% of the time
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))

#bake(prep(Amazon_recipe), new_data = NULL)

log_mod <- logistic_reg() %>% #Type of model
  set_engine("glm")

amazon_wf <- workflow() %>%
add_recipe(Amazon_recipe) %>%
add_model(log_mod) %>%
fit(data = amazon) # Fit the workflow

amazon_predictions <- predict(amazon_wf,
                              new_data=testData,
                              type="prob") #%>%
  #mutate(ACTION = ifelse(.pred1 > 0.75, 1, 0) # "class" gives 1s and 0s or "prob" gives probabilities
  #for changing cutoff ^

amazon_predictions_output <- amazon_predictions %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_predictions_output, file="amazon_preds.csv", delim=",")

### PENALIZED LOGISTIC REGRESSION ###
library(tidymodels)

penlog_mod <- logistic_reg(penalty = tune() , mixture = tune()) %>% #Type of model
  set_engine("glmnet") #Tells R what function to use

amazon_workflow <- workflow() %>%
add_recipe(Amazon_recipe) %>% #with target encoding
add_model(penlog_mod)

## Grid of values to tune over
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) #L-squared total tuning possibilities

## Split data for CV
folds <- vfold_cv(amazon, v = 10, repeats = 1)

## Run the CV
CV_results <- amazon_workflow %>%
tune_grid(resamples = folds,
          grid = tuning_grid,
          metrics = metric_set(roc_auc)) #area under ROC curve (false positives vs. true positives)

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("roc_auc")

## Finalize the Workflow & fit it
final_wf <- amazon_workflow %>%
finalize_workflow(bestTune) %>%
fit(data = amazon)

## Predict
pred_a <- final_wf %>%
predict(new_data = testData, type = "prob")

amazon_penlog <- pred_a %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_predictions_output, file="amazon_preds_penlog.csv", delim=",")
