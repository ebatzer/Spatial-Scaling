# Generating Biomass Samples for Matrix

setwd("C:/Users/ebatzer/Box Sync/Eviner lab shared/Evan/Research Projects/Spatial Scaling")

locationmatrix <- matrix(paste("X:" , rep(c(1:7),7)," Y:", sort(rep(c(1:7), 7)), sep = ''), nrow=7)
sampledmatrix <- sample(locationmatrix, 10, replace=F)

# write.csv("Biomass Sampling Locations 2017.csv", x = sampledmatrix)

locationmatrix <- matrix(paste("X:" , rep(c(0:8),8)," Y:", sort(rep(c(0:8), 8)), sep = ''), nrow=8)
sampledmatrix <- sample(locationmatrix, 64, replace=F)

#write.csv("Penetrometer Sampling Order 2017.csv", x = sampledmatrix)

locationmatrix <- matrix(paste("X:" , rep(c(1:7),7)," Y:", sort(rep(c(1:7), 7)), sep = ''), nrow=7)
sampledmatrix <- sample(locationmatrix, 10, replace=F)

data.frame(x = rep(c(1:8), 8), y = sort(rep(c(1:8), 8)))
sample(c(1:64), 10, replace = F)
