
#day 5
#bonga
#load obraries

library(tidyverse)
library(Rmisc)


#Load data

snakes <- read.csv("snakes.csv.txt")

#summary of data

snakes_summary <- snakes %>% 
mutate(day = as.factor(day))

snakes$day <- as.factor(snakes$day)



snakes_summary <- snakes %>% 
  group_by(day) %>%
  summarise(snake_mean = mean(openings),
            snakes_sd = sd(openings))

snakes_summary

snakes.summary <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

# Now we turn to some visual data summaries.


ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#possible null hypothesis


#H0: There is no difference between snakes with respect to 
# the number of openings at which they habituate.
#H0: There is no difference between days in terms of 
# the number of openings at which the snakes habituate
#snakes.aov <- aov(y ~ x, data = data)

# to test the days hypothesis

snakes.aov <- aov(openings ~ day, data = snakes)
summary(snakes.aov)

# to test both hypothesdis

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)

#testinf assumptions afterwards

#first visualize normality data
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check the tukey

snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)


#VISUALIZE THE FACTOR INTERACTION

ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings, 
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)

ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings, 
                          colour = snake)) +
  geom_boxplot(ase(fill = trap))

# Exercise  ---------------------------------------------------------------

# Get the moth data from GitHub
# Run a two


moth <- read.csv("moth.csv.txt") %>% 
  gather(key = "trap", value = "count", -Location )




moth$Location <- as.factor(moth$Location)



moth_summary <- moth %>% 
  group_by(Location) %>%
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_summary


moth.summary <- summarySE(data = moth, measurevar = "count", groupvars = c("Location"))

ggplot(data = moth, aes(x = Location, y = count)) +
  geom_segment(data = moth.summary, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#null hyothes


moth.aov <- aov(count ~ Location, data = moth)
summary(moth.aov)

moth.all.aov <- aov(count ~ Location * trap, data = moth)
summary(moth.all.aov)

moth.residuals <- residuals(moth.all.aov)
hist(moth.residuals)

plot(fitted(moth.all.aov), residuals(moth.all.aov))

# Check the tukey

moth.tukey <- TukeyHSD(moth.all.aov, which = "Location")
plot(moth.tukey)

ggplot(data = moth, aes(x = as.numeric(),
                          y = count, 
                          colour = moth)) +
  geom_line(size = 3) +
  geom_point(size = 4)




plt1 <- ggplot(data = moth, aes(x = Location, y = count)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.05, shape = 21)

plt2 <- ggplot(data = moth, aes(x = trap, y = count)) + 
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plt3 <- ggplot(moth, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap )) +
  geom_jitter(width = 0.05, shape = 21)

library(ggpubr)

ggarrange(plt1, plt2, plt3, labels = "AUTO",ncol = 2, nrow = 3)
#Regression

#Regressions test the statistical significance of the dependence of 
#one continuous variable on one ormany independent continuous variables.

#residuals(is the difference bwtween observed value and predicted vale(the line) )

#for the explanation of this statistical analysis 
#we are going to use the eruption data from o1' Faithful

#Look at the top of the data
head(faithful)


ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")


#hypothesis

#FORM A HYPOTHESIS
#H0: Waiting time doen NOT influence the duration of an eruption
#H1: waiting time does influence the duration of an eruption


faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


# Correlation -------------------------------------------------------------

library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv.txt")

# formulate an hypothesis

#H0: There is no relationship between stipe length and stipe mass
#For the kelp Ecklonia maxima
#H1: There is a relationship between stipe length and stipe mass
#For the kelp Ecklonia maxima


 # test a hypothesis
cor.test(ecklonia$stipe_diameter, ecklonia$stipe_length )

# visualize the data ------------------------------------------------------

ggplot(data = ecklonia, aes(x = stipe_diameter, y = stipe_length)) +
  geom_point()

# Run data tests at once --------------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(stipe_diameter:epiphyte_length)

eckonia_cor <- cor(ecklonia_sub)
eckonia_cor 


# Spearman rank test ------------------------------------------------------

ecklonia$length <- as.numeric(cut((ecklonia$stipe_diameter + ecklonia$stipe_diameter), 3))
# then run run Spearman test

cor.test(ecklonia$stipe_length, ecklonia$primary_blade_length, method = "spearman")

# Kendall rant test

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

#visualise all things

ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")



