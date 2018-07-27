library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)
library(gridExtra)
library(scales)
library(ggplot2)
library(lubridate)
library(plyr)
library(reshape2)
library(lattice)
library(ggthemes)
library(ggplot2)
library(ggmap)
library(ggalt)




# Funanani Thagulisi
# Dhiren Vanmari
# Bonga govuza
# Sasya Lagaa

#STUDY SITE

# Get hermanus Coordinates --------------------------------
Cape_whale_museum <- geocode("hermanus")  

# Get the Map ----------------------------------------------
# Google Satellite Map
hermanus_ggl_sat_map <- qmap("hermanus", zoom=12, source = "google", maptype="satellite")  
print(hermanus_ggl_sat_map)


# Google hybrid Map
hermanus_ggl_hybrid_map <- qmap("hermanus", zoom=12, source = "google", maptype="hybrid")  
print(hermanus_ggl_hybrid_map)

#Get Coordinates for hermanus's Places
Hermaneus_places <- c("hermanus")

## get longitudes and latitudes

places_loc <- geocode(Hermaneus_places)


### Google Hybrid Map ------
hermanus_ggl_hybrid_map + geom_point(aes(x=lon, y=lat),
                                    data = places_loc, alpha = 0.7, 
                                    size = 7,
                         color = "tomato") + 
  geom_encircle(aes(x=lon, y=lat),
                data = places_loc, size = 2, color = "red")



#REAL STUFF TO BE USED

#Statistics to be used

#levenes test of equality of error variences 

leveneTest(Limpet$`Oxygen consumption`~Limpet$Species*as.factor(Limpet$`Seawater Concentration`), center = mean )
table(levenes.limpet)

#TWO FACTOR ANOVA BEWEEN THE TWO LIMPET SPECIES ON OXYGEN CONSUMPTION DEPENDING ON SEAWATER CONCENTRATIONS

summary(aov(Limpet$`Oxygen consumption` ~ Limpet$Species * as.factor(Limpet$`Seawater Concentration`), data = filter(Limpet, Limpet$`Seawater Concentration` %in% c(50, 100))))

#T-TEST FOR THE DIFFERENCE IN OXYGEN CONSUMPTION BETWEEN LOTTIA SCABRA AND LOTTIA DIGITALIS
t.test(Limpet$`Oxygen consumption`~Limpet$Species, data = Limpet) 

#GRAPHICS
#Graphics between species

#
#boxplot between the species in their mean oxygen consumption
 boxplot(Limpet$`Oxygen consumption`~Limpet$Species, col = c("purple"), ylab= "oxygen consumption", xlab = "species", ylim= c(0,20), notch = FALSE, boxwex = 0.5)

#barplot disipiating the difference in oxygen consumption between the specie
barplot <- ggplot(Limpet, aes(factor(`Seawater Concentration`),`Oxygen consumption` , fill = Species)) + 
  geom_bar(stat="identity", position = "dodge") +  
  scale_fill_brewer(palette = "Set1") 
print(barplot)


##trying
ggplot(Limpet, aes(factor(`Seawater Concentration`),`Oxygen consumption` , fill = Species )) + 
   geom_bar(stat="identity", position = "dodge") + 
   scale_fill_brewer(palette = "Set1") +
   ylab("Oxygen consumption") + xlab("Seawater concentrations") +  
   theme(legend.title = element_blank()) + 
   scale_y_continuous(breaks = seq(0,20)) 

 
#graphis between oxygen consumption and seawater concentrations


#Lollipop Chart denoting the consumption of oxygen withi the two species 

Lollipop_chart <- ggplot(Limpet, aes(x=`Seawater Concentration`, y=`Oxygen consumption`)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=`Seawater Concentration`, 
                   xend=`Seawater Concentration`, 
                   y=0, 
                   yend=`Oxygen consumption`)) + 
  labs(title="LOLLIPOP CHART", 
       subtitle="saewater concetration VS oxygen intake ", 
       caption="source: limpets") + 
  theme(axis.text.x = element_text(angle=90, vjust=1.0))

print(Lollipop_chart)


#DENSITY PLOT
g <- ggplot(Limpet, aes(`Oxygen consumption`))
g + geom_density(aes(fill=factor(`Seawater Concentration`)), alpha=0.9) + 
  labs(title="Density plot", 
       subtitle="Density VS oxygen consumption",
       caption="Source: limpet",
       x="`Oxygen consumption`",y =  "Density`",
       fill="`Seawater Concentration`")

#BOXPLOT 


g3 <- ggplot(Limpet, aes(`Seawater Concentration`,`Oxygen consumption` ))
g3 + geom_boxplot(aes(fill=factor(`Seawater Concentration`))) + 
  theme(axis.text.x = element_text(angle=80, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="Oxygen consumption VS Seawater concentrations",
       caption="Source: limpet",
       x="Seawater Concentration%",
       y="Oxygen consumption")

#mean bar plot between oxygen consumption and seawater concetration

mean <- tapply(Limpet$`Oxygen consumption`, list(Limpet$`Seawater Concentration`), mean)
sem <- tapply(Limpet$`Oxygen consumption`, list(Limpet$`Seawater Concentration`), sd)/ sqrt(tapply(Limpet$`Oxygen consumption`, list(Limpet$`Seawater Concentration`), length))


mids <- barplot(mean, las = 1, font = 2, cex.axis = 1.5, cex.names = 1, ylim = c(0, 15))
mtext("Seawater concentration", side = 1, line = 3, cex = 1, font = 2)
mtext("Mean oxygen consumption", side = 2, line = 2.5, cex = 1, font = 2)
mtext("Mean oxygen consumption vs Seawater concentration", side = 3, line = 1, cex = 1, font = 2 )
arrows(x0= mids, y0= mean-sem, x1= mids, y1= mean+sem, code = 3, angle = 90, length = 0.15)

#trying for the last time

p3 <- ggplot(Limpet,
      aes(x = `Seawater Concentration`,
          y = `Oxygen consumption`)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))

p4 <- p3 + geom_point(aes(color = Species),
                      alpha = 0.5,
                      size = 15,
                      position = position_jitter(width = 20, height = 0.20))
print(p4)








