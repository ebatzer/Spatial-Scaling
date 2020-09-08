# Generating Biomass Samples for Matrix

setwd("../2018")

oldsamples <- read.csv("Biomass Sampling Locations 2018.csv")

locationmatrix <- c(matrix(paste("X:" , rep(c(1:7),7)," Y:", sort(rep(c(1:7), 7)), sep = ''), nrow=7))
locationmatrix <- locationmatrix[!locationmatrix %in% oldsamples$xy]
sampledmatrix <- sample(locationmatrix, 10, replace=F)

write.csv("../2019/Biomass Sampling Locations 2019.csv", x = sampledmatrix)

locationmatrix <- matrix(paste("X:" , rep(c(0:8),8)," Y:", sort(rep(c(0:8), 8)), sep = ''), nrow=8)
sampledmatrix <- sample(locationmatrix, 64, replace=F)

#write.csv("Penetrometer Sampling Order 2017.csv", x = sampledmatrix)
