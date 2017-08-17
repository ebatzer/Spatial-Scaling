setwd("~/Research Projects/Spatial Scaling")

mcl <- read.csv("MCLSpring2016Corrected.csv", stringsAsFactors = F)

head(mcl)

mcl.cov <- mcl[mcl$SubplotRes < 2,]

head(mcl.cov)

mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 1] <- ((0+1)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 2] <- ((1+5)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 3] <- ((5+25)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 4] <- ((25+50)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 5] <- ((50+75)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 6] <- ((75+95)/2)
mcl.cov[,8:(ncol(mcl.cov)-1)][mcl.cov[,8:(ncol(mcl.cov)-1)] == 7] <- ((95+100)/2)

coords <- read.csv("Plot Coordinates.csv")
head(coords)

coords$xmid <- rowSums(coords[,6:9]) / 4
coords$ymid <- rowSums(coords[,10:13]) / 4

coords$xmid
coords$ymid

coords.sample <- subset(coords,  scale == 1)
hist(dist(data.frame(coords.sample$xmid, coords.sample$ymid)))

cov.sample <- subset(mcl.cov, SubplotRes == 1)
head(cov.sample)

colnames(coords.sample)[1:2] <- c("Row", "SiteNo")
colnames(cov.sample)
cov.sample <- merge(cov.sample, coords.sample[,c(1:2, 14:15)])

library(vegan)
cov.sample[is.na(cov.sample)] <- 0

vd <- 1- vegdist(cov.sample[,9:53], na.rm=T, method="bray")
xy <- dist(cov.sample[,54:55])

library(ggplot2)
sampdat <- data.frame(dissimilarity = c(vd), coord = c(xy))
p1 <- ggplot(aes(y=dissimilarity, x=coord), data=sampdat)
p1 + geom_point() + stat_smooth()
