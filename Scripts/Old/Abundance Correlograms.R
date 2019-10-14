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
  
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$Grass,
                      increment=scaleval, resamp=500)
  cordat <- data.frame(scale = rep(scaleval, 
                                   length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("Grass", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  
  ####################################
  
  cov.sample$Forb <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Group == "Forb")])
               ]
  )
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$Forb,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, 
                                         length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("Forb", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  cov.sample$Legume <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Group == "Legume")])
               ]
  )
  
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$Legume,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, 
                                         length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("Legume", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  cov.sample$NativeCover <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Native")])
               ]
  )
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$NativeCover,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("NativeCover", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  cov.sample$NativeRichness <- specnumber(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Native")])
               ]
  )
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$NativeRichness,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("NativeRichness", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  cov.sample$ExoticCover <- rowSums(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Exotic")])
               ]
  )
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$ExoticCover,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("ExoticCover", 
                                         length(ncf.cor$correlation)),
                             corr = ncf.cor$correlation,
                             p = ncf.cor$p,
                             dist = ncf.cor$mean.of.class)
  )
  
  ####################################
  
  cov.sample$ExoticRichness <- specnumber(
    cov.sample[,which(colnames(cov.sample) %in% 
                        specs$Code[which(specs$Provenance == "Exotic")])
               ]
  )
  ncf.cor <- correlog(coords.sample$xmid , coords.sample$ymid, cov.sample$ExoticRichness,
                      increment=scaleval, resamp=500)
  cordat <- rbind(cordat, 
                  data.frame(scale = rep(scaleval, length(ncf.cor$correlation)),
                             site = rep(unique(coverdata$Site), 
                                        length(ncf.cor$correlation)),
                             class = rep("ExoticRichness", 
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
    xlab("Lag Distance") +
    ylab("Correlation") +
    xlim(0,scaleval * 20) + 
    facet_wrap(~class) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_color_viridis(discrete = T)
  
  ggsave(paste(unique(cordat$site), 
              "2nd run Spatial Correlation at scale",
              unique(cordat$scale),
               ".jpeg", 
               sep=''),
         height = 8,
         width = 16)
  
  ###############################
  
  plotdat <- cbind(cov.sample, coords.sample)
  
  plotdat <-plotdat[,c(ncol(plotdat), 
            ncol(plotdat)-1,
            ncol(cov.sample) : 
              (ncol(cov.sample) - 6),
            ncol(plotdat) -10,
            2
            )
          ]
  
  plotdat$plot <- c(1:nrow(plotdat))
  
  idvals <- colnames(plotdat)[3:9]
  
  plotdat <- reshape(plotdat, 
          varying = list(3:9),
          direction= "long")
  colnames(plotdat)[7] <- "value"
  
  newvec <- c()
  for( i in 1:length(plotdat$time)){
    newvec[i] <- idvals[plotdat$time[i]]
  }
  
  plotdat$time <- newvec
  
  p1 <- ggplot(aes(x=xmid, 
                   y=ymid, 
                   fill=value),
               data=plotdat)
  p1 + 
    geom_tile(size=unique(plotdat$scale)) +
    scale_fill_viridis() +
    facet_wrap(~time) +
    ggtitle(paste(unique(plotdat$Site),
                  "Spatial Pattern @",
                  unique(plotdat$scale)))
  
  ggsave(paste(unique(plotdat$Site),
               "2nd run Spatial Pattern at ",
               unique(plotdat$scale),
               ".jpeg", sep=''),
         height = 8,
         width = 16)

}

