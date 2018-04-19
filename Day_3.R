# Day_3.R
# Bonga Govuza
# 17 April 2018
# We are going to cover distribution and t tests

# Load library

library(fitdistrplus)
library(logspline)


#  ------------------------------------------------------------------------

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# uniform data
y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# T tests -----------------------------------------------------------------
 # tests copmaring two things, ANOVA more than two things
library(tidyverse)

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# check assumptions -------------------------------------------------------
# noramlity
# For this we can use the shapiro-wilk test
shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1]
shapiro.test(r_dat$dat)[2]
# but this  is testing  all of the data together
# we must be abit more clever about how we make this test

r_dat %>% 
  group_by(sample) %>%  
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
# Remember, the data are normal when p > 0.05
# The data are non normal when p <= 0.05
# for now we will simply say that this assumption is met when
# the variance of the samples sre not more than 2 - 4 times greater
# than one another

#chech everything at one
# wrong 
# or do it now

r_dat %>% 
  group_by(sample) %>%  
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))


# A one sample t test -----------------------------------------------------


r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

#Test normal distribution

shapiro.test(r_one$dat)

# perhaps a visualization 
# Run the test
t.test(r_one$dat, mu = 20)



#  Run a test we now know will produce s ignificant result
t.test(r_one$dat, mu = 30)


#  Pick a side ------------------------------------------------------------

t.test(r_one$dat, mu = 20, alternative = "less")

# or GREATER
t.test(r_one$dat, mu = 20, alternative = "greater")

# but what about  for the larger poluation of 30
# are the sample les than the population
t.test(r_one$dat, mu = 30, alternative = "less")

# what about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")

# Two sample test ---------------------------------------------------------

 # create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                           rnorm(n = 20, mean = 5, sd = 1)),
                   sample = c(rep("A", 20), rep("B", 20)))

# Run the basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Picka side
# Is A less B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

# Is A greater than B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

