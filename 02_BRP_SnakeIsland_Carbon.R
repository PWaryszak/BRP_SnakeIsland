#Get Data produced in 01_BRP_SnakeIsland_Data.R file:
#AHD_Elevation_m = Australian Height Datum (Correct Elevation)
library(tidyverse)

snake_data <- read.csv("snake_data.csv")
names(snake_data)


#Compute Carbon Stock (Mgha)=====
snake_data$SampleVolume.cm3      <- ((pi*(snake_data$CoreDiameter_mm/2 /10 )^2)*snake_data$LENGTH_TO_HORIZON_mm /10) #slice volume by 10 to get cm3 (cylinder formula for volume)
snake_data$dry_bulk_density.gcm3 <-  snake_data$DryWeight.g / snake_data$SampleVolume.cm3 
snake_data$CarbonDensity.gcm3    <- snake_data$dry_bulk_density.gcm3 * snake_data$C_percent/100
snake_data$CarbonStock.Mgha <- (((snake_data$CarbonDensity.gcm3  / 1000000 ) *100000000)  * snake_data$LENGTH_TO_HORIZON_mm /10 )

snake_data2 <- snake_data %>% 
  filter(solution.status == "FIX") 
  
#Data Checking (Volume ~Weight):
snake_west_plot1 <- ggplot(data = snake_data2,
                          aes(  y = SampleVolume.cm3, x = DryWeight.g )) +
  geom_point()+
  theme_bw()+
  theme(axis.text.x=element_blank(), #(size=10,colour = "black", angle=45, hjust = .8),
        axis.text.y=element_text(size=12,colour = "black"),
        axis.title.y=element_text(size=16,colour = "black",face = "bold",),
        axis.title.x=element_text(size=16, face = "bold", hjust = 0.5),
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text=element_text(size=11))

snake_west_plot1


#Elevation ~ C-Stock:
library(sjPlot)
tab_model(s1)
summary
  
s1<- lm(CarbonStock.Mgha~AHD_Elevation_m, data = snake_data2)

snake_west_plot2 <- ggplot(data = snake_data2,
                           aes(  y = AHD_Elevation_m, x = CarbonStock.Mgha, fill = site.y )) +
  geom_point(aes(fill=site.y))+
  labs(y ="Elevation (m)", x = bquote('C-stock ' (tonnes*~ha^-1)), fill = "Site: ")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,colour = "black"),
        axis.text.y=element_text(size=12,colour = "black"),
        axis.title.y=element_text(size=16,colour = "black",face = "bold",),
        axis.title.x=element_text(size=16, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.text=element_text(size=11))+
  ggtitle("Snake Island Only")

snake_west_plot2

library(gridExtra)
library(grid)
library(ggpubr)
ggarrange(snake_west_plot2,snake_west_plot1, nrow = 2)
