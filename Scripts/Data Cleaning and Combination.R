# Setting working directory:
setwd("~/Research Projects/Spatial Scaling")

# Calling necessary packages
library(vegan)
library(ggplot2)
library(viridis)

coords <- read.csv("Plot Coordinates.csv")
coords$xmid <- rowSums(coords[,6:9]) / 4
coords$ymid <- rowSums(coords[,10:13]) / 4


# Reading in cover data:
MCLdata <- read.csv("MCLSpring2016Corrected.csv", stringsAsFactors = F)
MCLdata[is.na(MCLdata)] <- 0
#MCLdata <- merge(MCLdata, coords)
head(MCLdata)

SFRECdata <- read.csv("SFRECSpring2016Corrected.csv", stringsAsFactors = F)
SFRECdata[is.na(SFRECdata)] <- 0
#SFRECdata <- merge(SFRECdata, coords)
head(SFRECdata)

HRECdata <- read.csv("HRECSpring2016Corrected.csv", stringsAsFactors = F)
HRECdata[is.na(HRECdata)] <- 0
#HRECdata <- merge(HRECdata, coords)
head(HRECdata)

nrow(MCLdata)
nrow(SFRECdata)
nrow(HRECdata)

newdat <- merge(MCLdata, SFRECdata, all=T)
newdat <- merge(newdat, HRECdata, all=T)
newdat[is.na(newdat)] <- 0

newdat <- newdat[newdat$SubplotRes < 2,]
newdat[is.na(newdat)] <- 0

newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 1] <- ((0+1)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 2] <- ((1+5)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 3] <- ((5+25)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 4] <- ((25+50)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 5] <- ((50+75)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 6] <- ((75+95)/2)
newdat[,10:(ncol(newdat)-1)][newdat[,10:(ncol(newdat)-1)] == 7] <- ((95+100)/2)


write.csv(x=newdat,
          file='SpringDataCombined.csv')

