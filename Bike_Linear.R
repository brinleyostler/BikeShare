#### LINEAR REGRESSION ####
## Set Up and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression") %>% 
  fit(formula=log(count)~weather+temp+humidity+windspeed, data=train)
my_linear_model


## Generate Predictions Using LInear Model
bike_predictions <- predict(my_linear_model,
                            new_data=test)  # Use fit to predict
bike_predictions

## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>% 
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=exp(count)) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime)))

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")

