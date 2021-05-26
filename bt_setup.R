# load package(s)
library(tidymodels)
library(tidyverse)

# seed
set.seed(57)

## load training data
money_train <- read_csv("data/train 2.csv") %>% 
  mutate(across(where(is.character), as.factor))

## load testing data
money_test <- read_csv("data/test.csv") %>% 
  mutate(across(where(is.character), as.factor))

## inspect data
skimr::skim_without_charts(money_train)

money_train %>% 
  select(money_made_inv, acc_now_delinq, acc_open_past_24mths, annual_inc, avg_cur_bal, bc_util, delinq_2yrs, delinq_amnt, dti, int_rate, loan_amnt, mort_acc, num_sats, num_tl_120dpd_2m, num_tl_30dpd, num_tl_90g_dpd_24m, out_prncp_inv, pub_rec, pub_rec_bankruptcies, tot_coll_amt, tot_cur_bal, total_rec_late_fee) %>% 
  cor(use = "complete.obs")

## create recipe
money_recipe <- recipe(money_made_inv ~ loan_amnt + out_prncp_inv + grade + num_tl_120dpd_2m + term,  data = money_train) %>% 
  step_other(all_nominal(), threshold = 0.2) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

## prep and bake recipe
money_recipe %>% prep(money_train) %>% bake(new_data = NULL)

## save objects required for tuning
save(money_fold, money_recipe, file = "data/money_setup.rda")

## load tuning objects
load(file = "data/bt_tune.rda")

## best tuning parameters
bt_tune %>% 
  select_best(metric = "rmse")

## train and fit model
workflow_tuned <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rmse"))

results <- fit(workflow_tuned, money_train)

## predict using test set
predictions <- results %>%
  predict(new_data = money_test) %>%
  bind_cols(money_test %>% select(id)) %>%
  select(id, .pred) %>% 
  rename(Id = id) %>% 
  rename(Predicted = .pred)

write_csv(predictions, "data/predictions.csv")
