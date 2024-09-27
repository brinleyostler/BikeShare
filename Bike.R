#### IMPORT LIBRARIES ####
library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)
library(poissonreg)
library(tidymodels)
library(glmnet)
library(stacks)

#
#### READ IN DATA ####
# read in train & test datasets with vroom()
train = vroom(file = "train.csv")
test = vroom(file = "test.csv")

#
#### EXPLORATORY DATA ANALYSIS ####
glimpse(train)

# Scatterplot of count over time
time_plot = ggplot(data = train, mapping = aes(x = datetime, y = count)) +
  geom_point() +
  geom_smooth(se = F)

# Scatterplot of count x temperature
temp_plot = ggplot(data = train, mapping = aes(x = temp, y = count)) +
  geom_point() +
  geom_smooth(se = F)

# Barplot of weather
weather_plot = ggplot(data = train, mapping = aes(x = weather)) +
  geom_bar()

# Scatterplot of count x humidity
humid_plot = ggplot(data = train, mapping = aes(x = humidity, y = count)) +
  geom_point() +
  geom_smooth(se = F)


# create a 4 panel ggplot (patchwork) showing 4 key features of the dataset
  # one panel must be a barplot of weather
(temp_plot + time_plot) / (humid_plot + weather_plot)



#### DATA CLEANING ####
train_clean = train %>% 
  select(-casual, -registered) %>% 
  mutate(count = log(count))

#### RANDOM FORESTS ####
forest_mod <- rand_forest(mtry = tune(),
                          min_n=tune(),
                          trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

#### FEATURE ENGINEERING ####
# Create and "bake" the recipe
bike_recipe = recipe(count~., data=train_clean) %>% 
  step_mutate(weather = ifelse(weather==4,3,weather)) %>%   # recode 4 to 3
  step_mutate(weather = factor(weather, levels=c(1,2,3),    # factor season
                               labels=c("clear", "mist", "precipitation"))) %>% 
  step_time(datetime, features="hour") %>%                  # extract hour
  step_mutate(datetime_hour = factor(datetime_hour,         # factor hour
                                     levels=c(0:23), labels=c(0:23))) %>% 
  step_mutate(season = factor(season, levels=c(1,2,3,4),    # factor weather
                              labels=c("spring", "summer", "fall", "winter"))) %>% 
  step_date(datetime, features="dow") %>%                   # extract day of week
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%                  # make dummy variables
  step_normalize(all_numeric_predictors())                  # mean = 0, sd = 1

prepped_recipe = prep(bike_recipe)
bike_baked = bake(prepped_recipe, new_data=train_clean)

## STACKED MODEL ####
# 3 models: linear, penalized, random forest
## Split data for CV
folds <- vfold_cv(train_clean, v = 5, repeats=1)

## Create a control grid
untuned_model <- control_stack_grid() #If tuning over a grid
tuned_model <- control_stack_resamples() #If not tuning a model

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5) ## L^2 total tuning possibilities

## Run the CV
preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control = untuned_model) # including the control grid in the tuning ensures you can
# call on it later in the stacked model

## Create other resampling objects with different ML algorithms to include in a stacked model, for ex
linear_model <- linear_reg() %>%
  set_engine("lm")

lin_reg_wf <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(bike_recipe)

lin_reg_model <- fit_resamples(
    lin_reg_wf,
    resamples = folds,
    metrics = metric_set(rmse, mae, rsq),
    control = tuned_model)

## Specify which models to include
my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model)


## Fit the stacked model
stack_mod <- my_stack %>%
  blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members() ## Fit the members to the dataset

## If you want to build your own metalearner you'll have to do so manually using
stackData <- as_tibble(my_stack)




#####
## MAKE PREDICTIONS ##
## Predict
## Use the stacked data to get a prediction
stack_mod %>% predict(new_data=test)

stacked_preds = predict(stack_mod, new_data = test)

## Format predictions for kaggle submission
recipe_kaggle_submission <- stacked_preds %>% 
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(datetime=as.character(format(datetime))) %>% 
  mutate(count = exp(count))

## Write out file
vroom_write(x=recipe_kaggle_submission, file="./StackedPreds.csv", delim=",")


