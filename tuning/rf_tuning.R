# Random forest tuning ----

# load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

# load required objects ----
load("data/money_setup.rda")

# define model ----
rf_model <- rand_forest(
  mode = "regression", 
  mtry = tune(), 
  min_n = tune()
) %>% 
  set_engine("ranger")

# check tuning parameters ----
parameters(rf_model)

# set-up tuning grid ----
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(2, 9)))

# define tuning grid ----
rf_grid <- grid_regular(rf_params, levels = 5)

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(money_recipe)

# tuning ----
tic("Random Forest")
rf_tune <- rf_workflow %>% 
  tune_grid(
    resamples = money_fold, 
    grid = rf_grid
  )
toc(log = TRUE)

# save runtime info ----
rf_runtime <- tic.log(format = TRUE)

# write out results & workflow ----
save(rf_tune, rf_workflow, rf_runtime, file = "data/rf_tune.rda")
