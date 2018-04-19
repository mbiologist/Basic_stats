# Day_4.R
# Bonga Govuza
# 19 April 2018
# We are going to cover distribution and t tests

library(tidyverse)
library(ggplot2)
library(ggpubr)


# t- test -----------------------------------------------------------------

chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)


# t - test ----------------------------------------------------------------

t.test(weight ~ Diet, data = chicks_sub)


# 1 way AVONA -------------------------------------------------------------




# Research question : is there a different in chicken  mass attianed after
# 21 days after the chickens having been feed four diferent diet

# Null hypothesis : there is no difference in chikens mass  at 21 days after
# having been fed one of diets

chicks_21 <- chicks %>%
  filter(Time == 21)
  
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)


ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(show.legend = FALSE, notch = TRUE) + theme_pubclean() +
  labs(y = "Chicken mass (g)") +
  theme(axis.text.x = element_text(face = "italic"))


# Tukey test --------------------------------------------------------------

TukeyHSD(chicks.aov1)

# AVONA -------------------------------------------------------------------

summary(aov(weight ~ Diet, data = chicks_21))

# Boxplot -----------------------------------------------------------------


#segments showing confidence intervals
chicks_turkey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_turkey$pairs <- as.factor(row.names(chicks_turkey))

ggplot(data = chicks_turkey) +
  geom_segment(aes(x = pairs, xend = pairs ,y = lwr, yend = upr)) + 
  geom_point(aes(x = pairs, y = diff))

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))


# Mutiple factor AVONA ----------------------------------------------------

# H0: there is no change in chicken mass from day 0 to day 21

chick_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 21))

ggplot(data = chick_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = TRUE,aes(fill = as.factor(Time)) )

# RUn an anova

# look only at day o and 21 for both time and diet

summary(aov(weight ~ as.factor(Time), data = chick_0_21))


TukeyHSD(aov(weight ~ as.factor(Time), data = chick_0_21))

plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chick_0_21))))

summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# look at the tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))
     
     
#creating a line graphics.off
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.xm = T))

#VISUALISE IT

ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) + 
  geom_point(shape = 15, size = 5)

#but what if we donty have normal data

kruskal.test(weight ~ Diet, data = chick_0_21)

library(pgirmess)

kruskalmc(weight ~ Diet, data = chick_0_21)



              