# Preliminary analysis of soil depth data
# Uses penetrometer data taken from McLaughlin reserve 4/22/2017

# Setting working directory:
setwd("C:/Users/Evan/Box Sync/Eviner lab shared/Evan/Research Projects/Spatial Scaling ")

# Loading packages:
library(ggplot2)
library(viridis)
library(vegan)
library(tidyverse)

# Reading in CSV:
soildepth <- read.csv("SoilDepthDataMCL.csv")
head(soildepth)

# Plotting as geom_tile:
p1 <- ggplot(aes(x= X, y= Y, fill=X17.5.cm), data=na.omit(soildepth))
p1 + 
  geom_tile() + 
  facet_wrap(~Plot) + 
  scale_fill_viridis() 

# Simple Mantel correlation:

# At what depth?
depth <- "X15.0.cm"

plotdist <- dist(soildepth[soildepth$Plot == 1,][,5:6])
depthdist <- dist(soildepth[soildepth$Plot == 1,colnames(soildepth) == depth])

mantel(x=plotdist, y=depthdist)
plot(mantel.correlog(depthdist, plotdist))
