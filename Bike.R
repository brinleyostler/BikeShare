library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

# read in train dataset with vroom()
train = vroom(file = "train.csv")


# perform an EDA
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

