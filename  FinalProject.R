###############################################
###### CPLN 675 Environmental Modelling #######
############ Date: May 13, 2020 ###############
###############################################

rm(list=ls())
setwd("~/Documents/675/Final")

install.packages("tidyverse")
install.packages("sf")
install.packages("raster")
install.packages("knitr")
install.packages("kableExtra")
install.packages("tidycensus")
install.packages("tigris")
install.packages("FNN")
install.packages("QuantPsyc")
install.packages("caret")
install.packages("yardstick")
install.packages("pscl")
install.packages("plotROC")
install.packages("ggrepel")
install.packages("pROC")
install.packages("grid")
install.packages("gridExtra")
install.packages("viridis")



library(tidyverse)
library(sf)
library(raster)
library(knitr)
library(kableExtra)
library(tidycensus)
library(tigris)
library(FNN)
library(QuantPsyc)
library(caret)
library(yardstick)
library(pscl)
library(plotROC) 
library(ggrepel)
library(pROC)
library(grid)
library(gridExtra)
library(viridis)

# 1. Setup map theme colors
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette2 <- c("#41b6c4","#253494")
palette4 <- c("#a1dab4","#41b6c4","#2c7fb8","#253494")
palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")
palette10 <- c("#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4",
               "#4eb3d3","#2b8cbe","#0868ac","#084081","#f7fcf0")

# 2. Data wrangling & feature engineering
# 2.1 Downland Land Cover

# 2.2 Land Cover Change Data
#setwd("C:/Users/ksteif/Desktop/spring_2017/theBook/sprawlChapter/data")
setwd("~/Documents/675/FinalRaleigh")
ReTria_Limit <- 
  st_read("ReTria_Limit.shp") %>%
  st_transform(102741)

lc_change = raster("resam4k.tif")

ggplot() +
  geom_sf(data=houstonMSA) +
  geom_raster(data=rast(lc_change) %>% na.omit %>% filter(value > 0), 
              aes(x,y,fill=as.factor(value))) +
  scale_fill_viridis(direction = -1, discrete=TRUE, name ="Land Cover\nChange") +
  labs(title = "Land Cover Change") +
  mapTheme()

# 2.3




# 2.4




# 2.5





