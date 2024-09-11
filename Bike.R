library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

#### READ IN DATA ####
# read in train & test datasets with vroom()
train = vroom(file = "train.csv")
test = vroom(file = "test.csv")


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

