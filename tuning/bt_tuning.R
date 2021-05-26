# Boosted tree tuning ----

# load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

# load required objects ----
load("data/money_setup.rda")

# define model ----
bt_model <- boost_tree(
  mode = "regression", 
  mtry = tune(), 
  min_n = tune(), 
  learn_rate = tune()
) %>% 
  set_engine("xgboost")

# check tuning parameters ----
parameters(bt_model)

# set-up tuning grid ----
bt_params <- parameters(bt_model) %>% 
  update(
    mtry = mtry(range = c(2, 10)), 
    learn_rate = learn_rate(range = c(-5, -0.2))
  )

# define tuning grid ----
bt_grid <- grid_regular(bt_params, levels = 5)

# workflow ----
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(money_recipe)

# tuning ----
tic("Boosted Tree")
bt_tune <- bt_workflow %>% 
  tune_grid(
    resamples = money_fold, 
    grid = bt_grid
  )
toc(log = TRUE)

# save runtime info ----
bt_runtime <- tic.log(format = TRUE)

# write out results & workflow ----
save(bt_tune, bt_workflow, bt_runtime, file = "data/bt_tune.rda")
