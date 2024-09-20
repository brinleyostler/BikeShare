## BASIC PLOT - 1 #####
# Load necessary library
library(ggplot2)

# Load the mtcars dataset
data("mtcars")
head(mtcars)

# Basic scatter plot: MPG vs Horsepower
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) +
  geom_point()

# This code creates a scatter plot of miles per gallon (mpg) versus horsepower 
  # (hp) using basic points. No labels or additional features are added yet.


## ADDING COLORS - 2 #####
# Scatter plot with color
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) +
  geom_point(color = "blue")

# Scatter plot with colors based on number of cylinders
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point()


## ADDING LABELS - 3 ######
# Scatter plot with labels
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  labs(color = "# of cylinders", title = "MPG vs Weight") +
  xlab("Car Weight") + ylab("Miles Per Gallon")


## ADDING A TRENDLINE - 4 ######
# Scatter plot with a curved trendline
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(se = FALSE, color = "black") +
  labs(color = "# of cylinders", title = "MPG vs Weight") +
  xlab("Car Weight") + ylab("Miles Per Gallon")


## ADDING A LINEAR TRENDLINE - 5 #######
# Scatter plot with a linear trendline
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(color = "# of cylinders", title = "MPG vs Weight") +
  xlab("Car Weight") + ylab("Miles Per Gallon")


## CHANGING THE THEME - 6 ######
# Scatter plot with a linear trendline and a different theme
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(color = "# of cylinders", title = "MPG vs Weight") +
  xlab("Car Weight") + ylab("Miles Per Gallon") +
  theme_bw()

