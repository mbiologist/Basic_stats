# Day_6.R
# 26 April 2018
# Confidence intervals


# load library ------------------------------------------------------------

library(rcompanion)
library(ggplot2)
library(tidyverse)

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

groupwiseMean(Steps ~ Teacher, data = data, conf = 0.95,digits = 3)

Sum <- groupwiseMean(data = data,
                     var = "Steps",
                     group = "Teacher",
                     conf = 0.95,
                     digits = 3)
ggplot( Sum, aes(x = Teacher, y = Mean)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = ))
  
data1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)


# Create A Graph ----------------------------------------------------------

ggplot(data = data1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper,
                    colour = Teacher)) + 
  facet_wrap(~Teacher)


# Testing assumptions or: How I learned to stop worrying and trans --------

chicks <- as_tibble(ChickWeight)

shapiro.test(chicks$weight)

chicks %>%
filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# Log_transform
Data2 <- data %>% 
  mutate(log_steps = log(Steps))

# Cube root
Data3 <- Data2 %>% 
  mutate(Cube_root = (Steps))


#Sq root
#Frequency
  
# Log_transform
dat1 <- data %>% 
  mutate(ln.step = log(Steps),
         ln10.step = log10(Steps),
         cube.step = Steps^(1/3),
         sqrt.step = sqrt(Steps)) %>% 
  select(-Student, -Rating) %>%
  gather(key = "data.type", value = "trans.data",
         -Sex, -Teacher) %>% 
  mutate(data.type = as.factor(data.type))



ggplot( data = dat1, aes(x = trans.data)) +
  geom_histogram(binwidth = 1000, aes(fill = Sex, position = "dodge")) +
  facet_grid(data.type ~ Teacher, scales = "free_x")

plot1 <- ggplot(data = filter(dat1, data.type == "Steps"), aes(x = trans.data)) + 
  geom_histogram(aes(fill = Sex), position =  "dodge")

plot2 <- ggplot(data = filter(dat1, data.type == "cube.step"), aes(x = trans.data)) + 
  geom_histogram(aes(fill = Sex), position =  "dodge")

plot3 <- ggplot(data = filter(dat1, data.type == "ln.step"), aes(x = trans.data)) + 
  geom_histogram(aes(fill = Sex), position =  "dodge")

plot4 <- ggplot(data = filter(dat1, data.type == "sqrt.step"), aes(x = trans.data)) + 
  geom_histogram(aes(fill = Sex), position =  "dodge")

plot5 <- ggplot(data = filter(dat1, data.type == "sqrt.step"), aes(x = trans.data)) + 
  geom_histogram(aes(fill = Sex), position =  "dodge")

library(ggpubr)

ggarrange(plot1, plot2, plot3, plot4, plot5, labels = "AUTO",ncol = 2, nrow = 3)


shapiro.test(iris$Petal.Width)

iris.dat <- as.tibble(iris)

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))

# Do a kruskal test -------------------------------------------------------

kruskal.test(Petal.Width ~ Species, data = iris)


