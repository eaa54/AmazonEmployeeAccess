library(tidymodels)
library(vroom)
library(tidyverse)
library(embed)

amazon <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/train.csv")
testData <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/test.csv")

amazon <- amazon %>%
  mutate(ACTION = as.factor(ACTION)) #make response a factor

Amazon_recipe <- recipe(ACTION ~ ., amazon) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>% #takes predictor columns and makes an other col for values that occur less than 1% of the time
  step_dummy(all_nominal_predictors())

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
