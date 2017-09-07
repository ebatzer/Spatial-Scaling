# Setting working directory:
setwd("~/Research Projects/Spatial Scaling")

# Calling necessary packages
library(vegan)
library(ggplot2)
library(viridis)
library(ncf)

# Reading in cover data:
coverdata <- read.csv("MCLSpring2016Corrected.csv", stringsAsFactors = F)
head(coverdata)

# Reading in plot coordinates:
coords <- read.csv("Plot Coordinates.csv")
head(coords)

# Reading in species list:
specs <- read.csv("SpatialScalingSpeciesList.csv")
head(specs)

unique(specs$Group)

# Taking only portions of the cover dataset that contain percent cover estimates
coverdata <- coverdata[coverdata$SubplotRes < 2,]
coverdata[is.na(coverdata)] <- 0

coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 1] <- ((0+1)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 2] <- ((1+5)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 3] <- ((5+25)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 4] <- ((25+50)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 5] <- ((50+75)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 6] <- ((75+95)/2)
coverdata[,9:(ncol(coverdata)-1)][coverdata[,9:(ncol(coverdata)-1)] == 7] <- ((95+100)/2)

# Establishing plot midpoints using coordinates

coords$xmid <- rowSums(coords[,6:9]) / 4
coords$ymid <- rowSums(coords[,10:13]) / 4

##### Generating betadisp ~ lag distance figures #####

scales <- c(.25,.5,1)

for( scaleval in scales){
  
  incrementval <- .25
  
  # Subsets for a given scale value
  coords.sample <- subset(coords,  scale == scaleval)
  cov.sample <- subset(coverdata, SubplotRes == scaleval)
  
  # Renames columns for merge
  plotdat <- cbind(cov.sample, coords.sample[, 14:15])
  plotdat[is.na(plotdat)] <- 0
  
  
  ####################################
  
  cov.sample$Grass <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Group == "Grass")])
               ]
  )
  
  cov.sample$Forb <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Group == "Forb")])
               ]
  )
  
  cov.sample$Legume <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Group == "Legume")])
               ]
  )
  
  cov.sample$ExoticCover <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Exotic")])
               ]
  )
  
  cov.sample$NativeCover <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Native")])
               ]
  )
  
  ####################################
  
  ncf.cor <- correlog(x = coords.sample$xmid , 
                      y = coords.sample$ymid, 
                      z = cov.sample$Grass,
                      w = cov.sample$Forb,
                      increment = incrementval, resamp=500)
  
  cordat <- data.frame(scale = rep(scaleval, 
                                   length(ncf.cor$correlation)),
                       site = rep(unique(coverdata$Site), 
                                  length(ncf.cor$correlation)),
                       class = rep("G x F", 
                                   length(ncf.cor$correlation)),
                       corr = ncf.cor$correlation,
                       p = ncf.cor$p,
                       dist = ncf.cor$mean.of.class)
  
  ####################################
  

  ncf.cor <- correlog(x = coords.sample$xmid , 
                      y = coords.sample$ymid, 
                      z = cov.sample$Grass,
                      w = cov.sample$Legume,
                      increment = incrementval, resamp=500)
  
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, 
                                         length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("G x L", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  

  ncf.cor <- correlog(x = coords.sample$xmid , 
                      y = coords.sample$ymid, 
                      z = cov.sample$Legume,
                      w = cov.sample$Forb,
                      increment = incrementval, resamp=500)
  
  
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, 
                                         length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("L x F", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  ncf.cor <- correlog(x = coords.sample$xmid , 
                      y = coords.sample$ymid, 
                      z = cov.sample$ExoticCover,
                      w = cov.sample$NativeCover,
                      increment = incrementval, resamp=500)
  
  
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, 
                                         length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("E x N", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  
  p1 <- ggplot(aes(x=dist, 
                   y=corr, 
                   color=p < .05),
               data=cordat)
  
  p1 + 
    geom_point( size=5, aes(color=NULL))+
    geom_point( size=4)+
    geom_line(aes(color = NULL)) + 
    ggtitle(paste(unique(cordat$class), 
                  "Spatial Correlation @ Scale",
                  unique(cordat$scale),
                  "m2")) +
    xlim(0,10) +
    ylim(-1,1) +
    xlab("Lag Distance") +
    ylab("Correlation") +
    facet_wrap(~class) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_color_viridis(discrete = T)
  
  ggsave(paste(unique(cordat$site), 
               " Cross Correlation at scale",
               unique(cordat$scale),
               ".jpeg", 
               sep=''),
         height = 8,
         width = 16)
  
  ###############################
  
}
  