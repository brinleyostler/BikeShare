#### DATA CLEANING ####
train_clean = train %>% 
  select(-casual, -registered) %>% 
  mutate(count = log(count))

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
  step_date(datetime, features="dow")                       # extract day of week
prepped_recipe = prep(bike_recipe)
bike_baked = bake(prepped_recipe, new_data=train_clean)

## CREATE WORKFLOW ##
# Define the linear model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Combine into workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=train_clean)

## MAKE PREDICTIONS ##
# Predict using workflow
lin_preds = predict(bike_workflow, new_data = test)
lin_preds

## Format predictions for kaggle submission
recipe_kaggle_submission <- lin_preds %>% 
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(datetime=as.character(format(datetime))) %>% 
  mutate(count = exp(count))

## Write out file
vroom_write(x=recipe_kaggle_submission, file="./LinearPreds.csv", delim=",")


