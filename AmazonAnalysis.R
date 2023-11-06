library(tidymodels)
library(vroom)
library(tidyverse)
library(embed)


# Read in Amazon employee access data
amazon <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/train.csv")
testData <- vroom("C:/Users/eaa54/Documents/School/STAT348/AmazonEmployeeAccess/test.csv")
view(amazon)

# Change response to a factor
amazon <- amazon %>%
  mutate(ACTION = as.factor(ACTION))

# Create recipe
Amazon_recipe <- recipe(ACTION ~ ., amazon) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_other(all_nominal_predictors(), threshold = 0.001) %>% #takes predictor columns and makes an other col for values that occur less than 1% of the time
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))#target encoding

#######################
##LOGISTIC REGRESSION##
#######################

# Create model
log_mod <- logistic_reg() %>% #Type of model
  set_engine("glm")

# Set workflow
amazon_wf <- workflow() %>%
add_recipe(Amazon_recipe) %>%
add_model(log_mod) %>%
fit(data = amazon)

# Make Predictions
amazon_predictions <- predict(amazon_wf,
                              new_data=testData,
                              type="prob") #%>%
  #mutate(ACTION = ifelse(.pred1 > 0.75, 1, 0) # "class" gives 1s and 0s or "prob" gives probabilities
  #for changing cutoff ^

# Format for Kaggle Competition
amazon_predictions_output <- amazon_predictions %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_predictions_output, file="amazon_preds.csv", delim=",")

#################################
##PENALIZED LOGISTIC REGRESSION##
#################################

# Create model
penlog_mod <- logistic_reg(penalty = tune() , mixture = tune()) %>% #Type of model
  set_engine("glmnet") #Tells R what function to use

# Set workflow
amazon_workflow <- workflow() %>%
add_recipe(Amazon_recipe) %>%
add_model(penlog_mod)

# Grid of values to tune over
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) #L-squared total tuning possibilities

# Split data for CV
folds <- vfold_cv(amazon, v = 10, repeats = 1)

# Run the CV
CV_results <- amazon_workflow %>%
tune_grid(resamples = folds,
          grid = tuning_grid,
          metrics = metric_set(roc_auc)) #area under ROC curve (false positives vs. true positives)

# Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("roc_auc")

# Finalize the Workflow & fit it
final_wf <- amazon_workflow %>%
finalize_workflow(bestTune) %>%
fit(data = amazon)

# Make predictions
pred_a <- final_wf %>%
predict(new_data = testData, type = "prob")

# Format for Kaggle
amazon_penlog <- pred_a %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_penlog, file="amazon_preds_penlog.csv", delim=",")

##################
##RANDOM FORESTS##
##################

# Create model
rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
          set_engine("ranger") %>%
          set_mode("classification") #predict binary outcome

# Set up Workflow
rf_workflow <- workflow() %>%
  add_recipe(Amazon_recipe) %>%
  add_model(rf_mod)

# Set up tuning values
rf_grid <- grid_regular(mtry(c(1,10)),
                        min_n(),
                        levels = 5)

# Set up k-fold cross validation and run it
rf_folds <- vfold_cv(amazon, v = 10, repeats = 1)

CV_rf_results <- rf_workflow %>%
  tune_grid(resamples = rf_folds,
            grid = rf_grid,
            metrics = metric_set(roc_auc)) #area under ROC curve (false positives vs. true positives)

# Find Best Tuning Parameters
bestTune_rf <- CV_rf_results %>%
  select_best("roc_auc")

# Finalize workflow and make predictions 
final_rf_wf <- rf_workflow %>%
  finalize_workflow(bestTune_rf) %>%
  fit(data = amazon)

pred_rf <- final_rf_wf %>%
  predict(new_data = testData, type = "prob")

# Format for Kaggle
amazon_rf <- pred_rf %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_rf, file="amazon_preds_rf.csv", delim=",")

###############
##NAIVE BAYES##
###############
library(discrim)
library(naivebayes)

# nb model
nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
      set_mode("classification") %>%
      set_engine("naivebayes") #need discrim library for this engine

# set workflow
nb_wf <- workflow() %>%
  add_recipe(Amazon_recipe) %>%
  add_model(nb_model)

# tune smoothness and Laplace here
# Set up tuning values
nb_grid <- grid_regular(Laplace(),
                        smoothness(),
                        levels = 5)

# Set up k-fold cross validation and run it
nb_folds <- vfold_cv(amazon, v = 5, repeats = 1)

CV_nb_results <- nb_wf %>%
  tune_grid(resamples = nb_folds,
            grid = nb_grid,
            metrics = metric_set(roc_auc)) #area under ROC curve (false positives vs. true positives)

# Find Best Tuning Parameters
bestTune_nb <- CV_nb_results %>%
  select_best("roc_auc")

# finalize workflow and fit it
final_nb_wf <- nb_wf %>%
  finalize_workflow(bestTune_nb) %>%
  fit(data = amazon)

pred_nb <- final_nb_wf %>%
  predict(new_data = testData, type = "prob")

# format for Kaggle
amazon_nb <- pred_nb %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_nb, file="amazon_preds_nb.csv", delim=",")

#######
##KNN##
#######

# knn model
knn_model <- nearest_neighbor(neighbors=50) %>% #can set or tune
  set_mode("classification") %>%
  set_engine("kknn")

knn_wf <- workflow() %>%
  add_recipe(Amazon_recipe_PCR) %>% #use other recipe for KNN alone
  add_model(knn_model) %>%
  fit(amazon)

knn_preds <- predict(knn_wf, new_data=testData, type="prob")

amazon_knn <- knn_preds %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_knn, file="amazon_preds_knn.csv", delim=",")

###########################################
##PRINCIPLE COMPONENT DIMENSION REDUCTION##
###########################################

Amazon_recipe_PCR <- recipe(ACTION ~ ., amazon) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.0000001) %>% #takes predictor columns and makes an other col for values that occur less than 1% of the time
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>% #target encoding
  step_pca(all_predictors(), threshold = 0.9)

# re-run naive bayes with principle component reduction
# nb model
nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes") #need discrim library for this engine

# set workflow
nb_wf <- workflow() %>%
  add_recipe(Amazon_recipe_PCR) %>%
  add_model(nb_model)

# tune smoothness and Laplace here
# Set up tuning values
nb_grid <- grid_regular(Laplace(),
                        smoothness(),
                        levels = 5)

# Set up k-fold cross validation and run it
nb_folds <- vfold_cv(amazon, v = 5, repeats = 1)

CV_nb_results <- nb_wf %>%
  tune_grid(resamples = nb_folds,
            grid = nb_grid,
            metrics = metric_set(roc_auc)) #area under ROC curve (false positives vs. true positives)

# Find Best Tuning Parameters
bestTune_nb <- CV_nb_results %>%
  select_best("roc_auc")

# finalize workflow and fit it
final_nb_wf <- nb_wf %>%
  finalize_workflow(bestTune_nb) %>%
  fit(data = amazon)

pred_nb <- final_nb_wf %>%
  predict(new_data = testData, type = "prob")

# format for Kaggle
amazon_nb <- pred_nb %>%
  mutate(ACTION = .pred_1, id = row_number()) %>%
  select(-.pred_0, -.pred_1) %>%
  relocate(id, .before = ACTION)

vroom_write(amazon_nb, file="amazon_preds_nb.csv", delim=",")

##NOTES ON RUNNING JOBS ON THE SERVER##
#save R objects
#save("filename.Rdata", list=c("amazon_wf"))
#load("amazon_wf")

#how to run something on becker

#R CMD BATCH --no save --no-restore AmazonAnalysis.R & 
#will give you a job number
#can check to see if it's running with top


#Parallel Processing#
#library(doParallel)
#parallel::detectCores() #my computer has 8 cores
#c1 <- makePSOCKcluster(num_cores)
#registerDoParallel(c1)

#CODE HERE

#stopCluster(c1)