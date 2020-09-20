#Install 2 R-packages we need if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")
#setwd("~/Documents/DeakinUni")#set working directory (where you store your data)

snake_core <- read.csv("SnakeIsland_Core.csv")
snake_elev <- read.csv("SnakeIsland_Elev.csv")

#Merge core and elevation date from above:
snake_data <- left_join(snake_core, snake_elev, by = "SampleID")
write.csv(snake_data, file = "snake_data.csv", row.names = F)

#PLOT elev versus latitude=====
snake_data <- read.csv("snake_data.csv")
names(snake_data)

ggplot(snake_data,aes(x=latitude, y=AHD_Height, color = site, shape = site))+ #AHD_Height = corrected elevation (m)
  #labs(x = "",y="",colour="year type")+
  geom_point(size = 3)+
  facet_grid(site~.,scale="fixed") +
  theme_bw()+
    coord_flip()+
    theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
        strip.text=element_text(size=24))

