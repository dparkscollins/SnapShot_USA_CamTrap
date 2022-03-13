install.packages("readxl")
install.packages("diplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tidyverse")

library(readxl)
library(dplyr) 
library(ggplot2)
library(lubridate)
library(tidyverse)

### Read in SnapshotUSA data from excel file
Snapshot2021 <- read_excel("/Users/danielcollins/github/SnapShot_USA_CamTrap/SSFA2021.xlsx")

#Exclude Unknown Species, Mammal, Human-Pedestrian, Human-Maintenance crew, 
#Human-Camera Trapper, and Blank from common_name
Snapshot2021<-filter(Snapshot2021, common_name !="Unknown species", 
                     common_name !="Mammal", common_name !="Human-Pedestrian"
                     , common_name !="Human-Maintenance crew", common_name !=
                       "Human-Camera Trapper", common_name !="Blank") 


### CREATE FIRST GGPLOT
ggplot(data = Snapshot2021, mapping = aes(x=start_time, y=common_name)) + 
  geom_point(mapping = aes(color = common_name)) + theme(legend.position = "none", 
                                                         panel.background = element_blank())
  
  


                     
### This converts time of start of sequence ($start_time) into digital hours -- I find that easier for plotting
Snapshot2021$date <-as.Date(Snapshot2021$start_time)
Snapshot2021$phototime<-as.POSIXct(Snapshot2021$start_time, format = "%H:%M")
Snapshot2021$phototime <- hour(Snapshot2021$phototime) + minute(Snapshot2021$phototime)/60 + second(Snapshot2021$phototime)/3600

### calculate sunrise and sunset times for your location (need to install package suncalc)
install.packages("suncalc")
library(suncalc) 
library(scales)
sun <-getSunlightTimes(
    date = seq.Date(as.Date("2021-08-30"), as.Date("2021-11-02"), by = 1),   	           # change dates to your start and end dates
    keep = c("sunrise", "sunset"),	
    lat = 35.808047,                                                                        # latitude of your subproject
    lon = -80.886671,                                                     	           # longitude of your subproject
    tz = "America/New_York"                                                	           # time zone of your subproject
  )
sun$sunrise<-as.POSIXct(sun$sunrise, format = "%H:%M")
sun$sunrise <- hour(sun$sunrise) + minute(sun$sunrise)/60 + second(sun$sunrise)/3600
sun$sunset<-as.POSIXct(sun$sunset, format = "%H:%M")
sun$sunset <- hour(sun$sunset) + minute(sun$sunset)/60 + second(sun$sunset)/3600
sun$dawn1<- sun$sunrise - 1                                                      	   # the following lines create 6 min ribbons to create gray shadign 1 hour before sunrise
sun$dusk1<- sun$sunset + 1                                                   	           # and one hour after sunset
sun$dawn2<- sun$sunrise - 0.9                                                     	   
sun$dusk2<- sun$sunset + 0.9    
sun$dawn3<- sun$sunrise - 0.8                                                      	   
sun$dusk3<- sun$sunset + 0.8    
sun$dawn4<- sun$sunrise - 0.7                                                      	   
sun$dusk4<- sun$sunset + 0.7
sun$dawn5<- sun$sunrise - 0.6                                                     	   
sun$dusk5<- sun$sunset + 0.6
sun$dawn6<- sun$sunrise - 0.5                                                     	   
sun$dusk6<- sun$sunset + 0.5    
sun$dawn7<- sun$sunrise - 0.4                                                      	   
sun$dusk7<- sun$sunset + 0.4    
sun$dawn8<- sun$sunrise - 0.3                                                      	   
sun$dusk8<- sun$sunset + 0.3
sun$dawn9<- sun$sunrise - 0.2                                                     	   
sun$dusk9<- sun$sunset + 0.2      
sun$dawn10<- sun$sunrise - 0.1                                                     	   
sun$dusk10<- sun$sunset + 0.1      


speciesplot<-filter(Snapshot2021, common_name=="White-tailed Deer") # change species name to filter for a different species
speciesplot<- full_join(Snapshot2021,sun, by="date")

### Note, the font, line and marker dimension here are set so that I can resize the plot to 2000 pixels wide in R-Studio before exporting as png.
### geom_ribbons used to plot gray areas for dawn, dusk and dark hours
ggplot(speciesplot, aes(x = date, y = phototime)) + ggtitle("White-tailed Deer") +                   # change species name to change plot title
  scale_x_date(date_minor_breaks = "1 day", date_labels = "%b %d", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits=c(0,24), breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), expand = c(0, 0)) +
  geom_ribbon(aes(ymin=sunset, ymax=sunrise), 						# 6 min wide ribbons for progressive shading at dawn and dusk
              fill="white") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk1), 
              fill="gray70") +   #fill color
  geom_ribbon(aes(ymin=dawn1, ymax=sunrise), 
              fill="grey70") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk2), 
              fill="gray73") +   #fill color
  geom_ribbon(aes(ymin=dawn2, ymax=sunrise), 
              fill="grey73") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk3), 
              fill="gray76") +   #fill color
  geom_ribbon(aes(ymin=dawn3, ymax=sunrise), 
              fill="grey76") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk4), 
              fill="gray79") +   #fill color
  geom_ribbon(aes(ymin=dawn4, ymax=sunrise), 
              fill="grey79") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk5), 
              fill="gray82") +   #fill color
  geom_ribbon(aes(ymin=dawn5, ymax=sunrise), 
              fill="grey82") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk6), 
              fill="gray85") +   #fill color
  geom_ribbon(aes(ymin=dawn6, ymax=sunrise), 
              fill="grey85") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk7), 
              fill="gray88") +   #fill color
  geom_ribbon(aes(ymin=dawn7, ymax=sunrise), 
              fill="grey88") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk8), 
              fill="gray91") +   #fill color
  geom_ribbon(aes(ymin=dawn8, ymax=sunrise), 
              fill="grey91") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk9), 
              fill="gray94") +   #fill color
  geom_ribbon(aes(ymin=dawn9, ymax=sunrise), 
              fill="grey94") +   #fill color
  geom_ribbon(aes(ymin=sunset, ymax=dusk10), 
              fill="gray97") +   #fill color
  geom_ribbon(aes(ymin=dawn10, ymax=sunrise), 
              fill="grey97") +   #fill color
  geom_ribbon(aes(ymin=dusk1, ymax=24), 
              fill="gray67") +   #fill color
  geom_ribbon(aes(ymin=0, ymax=dawn1), 
              fill="grey67") +   #fill color
  geom_point(mapping = aes(color = common_name)) +    						# dots for photo capture times
  theme_bw(base_size = 40) +
  theme(axis.ticks = element_line(colour = "gray50", size = 2), panel.border = element_rect(colour = "gray50", fill=NA, size=2), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Date", y = "Hour (24 hour clock)")

##Clean the plot above






##Finish this code by looking at putting multiple plots on same page, but look at fixing dimensions
#Put 2 plots on same page
install.packages("ggpubr")
library(ggpubr)

ggarrange(deer, coyote + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 2)











