#### POISSON REGRESSION ####
train_clean = train %>% 
  mutate(weather = as.factor(weather))
test_clean = test %>% 
  mutate(weather = as.factor(weather))

my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized linear model
  set_mode("regression") %>%
  fit(formula=count~datetime+season+holiday+workingday+
        weather+temp+atemp+humidity+windspeed, data=train_clean)

## Generate predictions using poisson model
bike_predictions <- predict(my_pois_model,
                            new_data=test_clean)
bike_predictions

## Format predictions for kaggle submission
pois_kaggle_submission <- bike_predictions %>% 
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(datetime=as.character(format(datetime)))

## Write out file
vroom_write(x=pois_kaggle_submission, file="./LinearPreds.csv", delim=",")


