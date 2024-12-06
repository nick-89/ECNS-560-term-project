library(tidyverse)
library(tidymodels)
library(glmnet)
library(xgboost)
library(vip)
data1=read.csv("acs_cacensus2.csv")
data2 = data1 %>%
  mutate(across(where(is.integer), as.factor))
data3 = data2 %>%
  select(!X) %>%
  drop_na() 

str(data3)
set.seed(5942)
sampled_data = data3 %>%
  slice_sample(prop = 0.1)
shelter_split=initial_split(sampled_data,prop = .8)
shelter_train=shelter_split%>%
  training()
shelter_test=shelter_split%>%
  testing()

shelter_recipe = recipe(shelter_cost ~ ., data = shelter_train) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors(),freq_cut = 85/15,unique_cut = .05)
  
xgb_spec = boost_tree(
  trees = 1000,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


xgb_workflow <- workflow() %>%
  add_recipe(shelter_recipe) %>%
  add_model(xgb_spec)


xgb_grid <- grid_space_filling(
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  size = 20
)


shelter_folds = vfold_cv(shelter_train, v = 5)


xgb_tune_results = tune_grid(
  xgb_workflow,
  resamples = shelter_folds,
  grid = xgb_grid,
  metrics = metric_set(rmse, rsq, mae)
)


best_params = select_best(xgb_tune_results, metric = "rmse")


final_xgb_workflow = xgb_workflow %>%
  finalize_workflow(best_params)


final_xgb_fit = final_xgb_workflow %>%
  fit(data = shelter_train)


xgb_preds = predict(final_xgb_fit, shelter_test)


test_metrics = bind_cols(
  shelter_test %>% select(shelter_cost),
  predictions = xgb_preds
) %>%
  metrics(truth = shelter_cost, estimate = .pred)




