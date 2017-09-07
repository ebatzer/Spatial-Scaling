# Setting working directory:
setwd("~/Research Projects/Spatial Scaling")

# Calling necessary packages
library(vegan)
library(ggplot2)
library(viridis)

# Reading in cover data:
coverdata <- read.csv("HRECSpring2016Corrected.csv", stringsAsFactors = F)
head(coverdata)

# Reading in plot coordinates:
coords <- read.csv("Plot Coordinates.csv")
head(coords)

# Taking only portions of the cover dataset that contain percent cover estimates
coverdata <- coverdata[coverdata$SubplotRes < 2,]

coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 1] <- ((0+1)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 2] <- ((1+5)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 3] <- ((5+25)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 4] <- ((25+50)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 5] <- ((50+75)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 6] <- ((75+95)/2)
coverdata[,8:(ncol(coverdata)-1)][coverdata[,8:(ncol(coverdata)-1)] == 7] <- ((95+100)/2)

# Establishing plot midpoints using coordinates
coords$xmid <- rowSums(coords[,6:9]) / 4
coords$ymid <- rowSums(coords[,10:13]) / 4

##### Generating betadisp ~ lag distance figures #####

scales <- c(.25,.5,1)
scaleval <- .25
counter <- 1
for( scaleval in scales){
  
  # Subsets for a given scale value
  coords.sample <- subset(coords,  scale == scaleval)
  cov.sample <- subset(coverdata, SubplotRes == scaleval)
  
  # Renames columns for merge
  plotdat <- cbind(cov.sample, coords.sample[, 14:15])
  plotdat[is.na(plotdat)] <- 0
  
  vd <- 1- vegdist(plotdat[,9:ncol(cov.sample)], na.rm=T, method="bray")
  xy <- dist(plotdat[,c((ncol(plotdat)-1) : ncol(plotdat))])
  
  if(counter == 1){
  
    betadat <- data.frame(dissimilarity = c(vd),
                          coord = c(xy),
                          scale = rep(scaleval, length(vd)),
                          site = rep(unique(plotdat$Site), length(vd))
                          )
    
  }
  if(counter > 1){
    
    betadat <- rbind(betadat,
                    data.frame(dissimilarity = c(vd),
                               coord = c(xy),
                               scale = rep(scaleval, length(vd)),
                               site = rep(unique(plotdat$Site), length(vd)))
                    )
    
  }
  counter <- counter + 1
}


############## Predicted values for GGplot ###################

#Distance-dissimilarity following Millar et al. (2011) was fit using 
m1 <- glm( dissimilarity ~ coord, data=betadat[betadat$scale == 1,], family = binomial(link="log"))

m2 <- glm( dissimilarity ~ coord, data=betadat[betadat$scale == .5,], family = binomial(link="log"))

m3 <- glm( dissimilarity ~ coord, data=betadat[betadat$scale == .25,], family = binomial(link="log"))

xval <- 20

predval <- c(predict(m1, newdata = data.frame(coord = seq(0,xval, by=.25))),
             predict(m2, newdata = data.frame(coord = seq(0,xval, by=.25))),
             predict(m3, newdata = data.frame(coord = seq(0,xval, by=.25))))

predval <- exp(predval)

predicted.data <- data.frame(dissimilarity = predval,
                             coord = rep(seq(0,xval, by=.25), 3),
                             scale = c(rep(1,length(seq(0,xval, by=.25))),
                                       rep(.5,length(seq(0,xval, by=.25))),
                                       rep(.25,length(seq(0,xval, by=.25)))),
                             site = rep(unique(betadat$site), length(predval)))

#### Plotting Data ####

p1 <- ggplot(aes(x= coord, 
                 y= dissimilarity, 
                 fill=factor(scale)), 
             data=betadat)
p1 +
  geom_boxplot(aes(x=cut(coord,seq(0, 20, by=1)))) +
  ggtitle(paste(unique(betadat$site), "Bray-Curtis dissimilarity")) +
  xlab("Lag Distance") +
  ylab("1 - BC Dissimilarity") +
  facet_wrap(~scale) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis(discrete=T) +
  # GLM estimation:
  geom_line(data=predicted.data, color="black", size=1.5)

ggsave(paste(unique(betadat$site), "Bray-Curtis dissimilarity boxplot.jpeg"),
       height = 8,
       width = 16)


p2 <- ggplot(aes(x= coord, 
                 y= dissimilarity,
                 fill=factor(scale)), 
             data=betadat)

p2 +
  theme_bw() +
  geom_point(size=2, pch=21, color="gray30")+
  ggtitle(paste(unique(betadat$site), "Bray-Curtis dissimilarity")) +
  xlab("Lag Distance") +
  ylab("1 - BC Dissimilarity") +
  facet_wrap(~scale) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis(discrete=T, alpha = .5, option="viridis") + 
  xlim(0,xval)+
  # GLM estimation:
  geom_line(data=predicted.data, color="black", size=1.5)

ggsave(paste(unique(betadat$site), "Bray-Curtis dissimilarity points.jpeg"),
       height = 8,
       width = 16)

# Mantel Tests

scales <- c(.25,.5,1)

for( scaleval in scales){
  
  # Subsets for a given scale value
  coords.sample <- subset(coords,  scale == scaleval)
  cov.sample <- subset(coverdata, SubplotRes == scaleval)
  
  # Renames columns for merge
  plotdat <- cbind(cov.sample, coords.sample[, 14:15])
  plotdat[is.na(plotdat)] <- 0
  
  vd <- vegdist(plotdat[,9:ncol(cov.sample)], na.rm=T, method="bray")
  xy <- dist(plotdat[,c((ncol(plotdat)-1) : ncol(plotdat))])
  
  if(scaleval == scales[1]){
    
    mtest <- mantel.correlog(D.eco = vd, 
                             D.geo = xy, 
                             break.pts = seq(0, 20, by=1), 
                             cutoff = F)
    mandat <- data.frame(mtest$mantel.res, 
                         scale=rep(scaleval, mtest$n.class),
                         site = rep(unique(plotdat$Site), mtest$n.class)
    )
    
  }else{
    mtest <- mantel.correlog(D.eco = vd, 
                             D.geo = xy, 
                             break.pts = seq(0, 20, by=1), 
                             cutoff = F)
    
    mandat <- rbind(mandat,
                    data.frame(mtest$mantel.res, 
                         scale=rep(scaleval, mtest$n.class),
                         site = rep(unique(plotdat$Site), mtest$n.class)
                         ))
    
  }
  
}


p1 <- ggplot(aes(x= class.index, 
                 y= Mantel.cor, 
                 color=Pr.corrected. < .05), 
             data=mandat)

p1 +
  geom_point( size=5, aes(color=NULL))+
  geom_point( size=4)+
  geom_line(aes(color = NULL)) + 
  geom_smooth(method=lm, formula = y ~ x + I(x^2), aes(color = NULL))+
  ggtitle(paste(unique(mandat$site), "Mantel Correlation")) +
  xlab("Lag Distance") +
  ylab("Mantel Correlation") +
  facet_wrap(~scale) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_viridis(discrete = T)

ggsave(paste(unique(mandat$site), "Mantel Correlation.jpeg"),
       height = 8,
       width = 16)


