# Day_1.R
# This is a stats script
# purpose: to practice somwe of the concerpts that we will encounter
# 12 April 2018

library(tidyverse)

# Integers ----------------------------------------------------------------

# generate some integer data
integer_r <- as.integer(seq(5, 14, by = 1))

# Look at a summary of integers
summary(integer_r)

# Contiuous ---------------------------------------------------------------

# Generating a sequences of numbers
numeric_r <- seq(23, 43, length.out = 10)

# Dates -------------------------------------------------------------------

# one may perform some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-12")
# or for example
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
# There is much more
summary(dates_r)

# Dataframes --------------------------------------------------------------

# Create the base dataframe
df_r <- data.frame(integers = integer_r,
                      numeric = numeric_r,
                      dates = dates_r)
# Then upgrade it to a tible
# To successful create a tible the legnth of column same have the same legnth/ date, int, and num must have the same legnth
df_r <- as_tibble(df_r)
summary(df_r)

 

# Categories --------------------------------------------------------------

# Electronics
elect_r <- as.factor (c("laptops",
                        "desktops",
                        "cell phone"))

# People
people_r <- as.factor(c("funny",
                        "beautiful",
                        "beanies"))

# Colour
colour_r <- as.factor(c("red", "blue"))


# Ordinal data ----------------------------------------------------------

# Here we still have qualitative data
# but with some sort of order

colour_qual <- ordered(c("blue", "green",
                         "yellow", "orange",
                         "red"),
                       levels = c("blue", "green",
                                  "yellow", "orange",
                                  "red"))


# binary data -------------------------------------------------------------

# These are generally represented as : True or False

binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# character data ----------------------------------------------------------

sites_r <- c("yztervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")

# Missing value -----------------------------------------------------------

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)

# summary
summary(chicks_nest)

# the mean
mean(chicks_nest)

# The Standard deviation
sd(chicks_nest)

chicks <- as_tibble(ChickWeight)


# Descriptive statistics --------------------------------------------------

# First create a dataframe
chicks <- as_tibble(ChickWeight)

# count the data
chicks %>% 
  summarise(chicken_count = n())
# or 
nrow(chicks)

#  Measure of central tendency------------------------------------------------------------------------

# Calculate mean weight
chicks %>% 
  summarise(mean_wt = mean(weight))

# Be more specific --------------------------------------------------------

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))
  

# Visualise the density data
ggplot(data = filter(chicks, Time == 21),
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)

# Stewness ----------------------------------------------------------------

# Calculate the numeric value
library(e1071)

#compare difference in mean and median 
chicks %>% 
filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))


# Kurtosis ---------------------------------------------------------------


# calculate the 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            skewness(faithful$eruptions))


# Measures of variation and spread ----------------------------------------

# Below is a summary of different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>%
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25), 
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))




