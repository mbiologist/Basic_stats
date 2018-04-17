# Day_2.R
# Bonga Govuza
# 13 April 2018
# The day in which we discuss data visualization and distributions


# Load library ------------------------------------------------------------

library(tidyverse)


# How do we calculate mean, SD, var, median etc...


# Manual calculation ------------------------------------------------------

# The mean
 
r_dat <- data.frame(dat = rnorm(n = 600, mean = 732, sd = 50), 
                    sample = "A")

# quick visualization
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

# The meqan
# sum of all the points
# divided by
# the umber of all the points
r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))

# The median

order(r_dat$dat)[(length(r_dat$dat)+1)/2]

# or use tidy

r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# Or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))


# Variance ----------------------------------------------------------------

# The sum of 
  # Each value 
    # minus 
      # the mean
        # Squared
# Divided by 
  # The count of samples minus one

r_dat %>%
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            r_var_func = var(dat))

# The Standard deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))


# Exercise ----------------------------------------------------------------

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
          quart_1 = quantile(weight, 0.25),
          med_weight = median(weight),
          mean_weight = mean(weight),
          quart_3 = quantile(weight, 0.75),
          max_weight = max(weight))


# Visualization -----------------------------------------------------------

# First load our libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis) # it gives some nice palace 


# Qualitative -------------------------------------------------------------
# load our SA time data
sa_time <- read.csv("SA_time.csv")

# edit our data 


sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("cape town", "George", "PE"), times = 6),
                 rep("joburg", 2)))

#create long data
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)

# Qualitative -------------------------------------------------------------

# create a count of qualitative values
sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))


#  Stacked bar graphs
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "count") +
  theme_minimal()

# Stacked proportion bar graphs

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "proportion") +
  theme_minimal()


# A pie chart...

ggplot(data = sa_count, aes(x = "", y =n, fill = time_type)) + 
  geom_bar(width = 1, stat = "identity") + 
  labs(title = "pie chart", subtitle = "but why though",
       x = NULL, y = NULL) + 
  coord_polar("y", start = 0) + 
  theme_minimal()

# side by side bar graph (the graph with separated bars)
ggplot(data = sa_long, aes(x = time_type, fill = time_type)) + 
  geom_bar(show.legend = FALSE) + 
  labs(title = "sa_time data", subtitle = "nothing really",
       x = "time_type" , y = "Time(minutes)") + 
  theme_minimal()

# Contiuous data ----------------------------------------------------------

# Histograms 

ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# oh no!!!
# Lets get rid of that one value

sa_clean <- sa_long %>% 
  filter(minutes < 300)

# Try again 
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.. , fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplots ----------------------------------------------------------------

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))



# Notched box -------------------------------------------------------------


ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)


# calculate summary stats for plotting over the boxplot -------------------

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# plot the mean -----------------------------------------------------------

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")


# relationaships ----------------------------------------------------------



# A Basic scatterplot

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim =  c(0,60), ylim = c(0,60))


# Adding trend lines ------------------------------------------------------

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  coord_equal(xlim =  c(0,60), ylim = c(0,60))

   #Adding trend lines
  ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
    geom_point(aes(colour = geo)) +
    geom_smooth(aes(colour = geo), method = "lm") +
    coord_equal(xlim = c(0, 60), ylim = c(0, 60))
  
  










