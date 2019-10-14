# Set working directory:
setwd("~/Research Projects/Spatial Scaling")

# Read in dataset:
comdata <- read.csv("HRECSpring2016Corrected.csv",
                    header=T,
                    stringsAsFactors=F)

# Checking data:
str(comdata) #All species data should be an integer
head(comdata)

# Calculating species richness at each scale:
for( rownumber in 1:nrow(comdata)){
  
  # Plots of resoltion < 1 have all species accounted for
  if(comdata$SubplotRes[rownumber] %in% c(.25,.5,1)){
    comdata[rownumber, 8:ncol(comdata)][
      which(comdata[rownumber, 8:ncol(comdata)] != 0)] <- 
      1
    
    comdata[rownumber, 8:ncol(comdata)][
      which(is.na(comdata[rownumber, 8:ncol(comdata)]))] <- 
      0
    
  # Plots of resolution 2x2 need to contain all species from 1m2 plots
  }else if(comdata$SubplotRes[rownumber] == 2){ 
    comdata[rownumber, 8:ncol(comdata)] <-
      colSums(comdata[comdata$SiteNo == comdata$SiteNo[rownumber] &
                        comdata$PlotNo == comdata$PlotNo[rownumber]&
                        comdata$SubplotRes %in% c(1,2), 
                    8:ncol(comdata)], na.rm = T)
    comdata[rownumber, 8:ncol(comdata)][
      which(comdata[rownumber, 8:ncol(comdata)] != 0)] <- 
      1
    
  # Plots of resolution 4x4 need to contain all species from 4m2 plots
  }else if(comdata$SubplotRes[rownumber] == 4){ 
    comdata[rownumber, 8:ncol(comdata)] <-
      colSums(comdata[comdata$SiteNo == comdata$SiteNo[rownumber] &
                        comdata$PlotNo %in% c(comdata$PlotNo[rownumber],
                                              comdata$PlotNo[rownumber]-1
                                              )&
                        comdata$SubplotRes %in% c(2,4), 
                      8:ncol(comdata)], na.rm = T)
    comdata[rownumber, 8:ncol(comdata)][
      which(comdata[rownumber, 8:ncol(comdata)] != 0)] <- 
      1
  }
}

# Pulling out data from just 4x4m plots
fours <- comdata[comdata$SubplotRes == 4,]

# Removing those rows (to be replaced)
comdata <- comdata[- (comdata$SubplotRes == 4),]

# Calculating species richness at 8x8m plot scale
for( sites in unique(fours$SiteNo)){
  
  newrow <- c(fours[1,1:3], 1, 8, 1,1)
  newrow <- c(newrow, colSums(
    fours[fours$SiteNo == sites, 8:ncol(fours)]
    ))
  newrow[8:length(newrow)][
    which(newrow[8:length(newrow)] > 1)
    ] <- 1
  names(newrow) <- colnames(fours)
  fours <- rbind(fours, newrow)
}

# Calculating species richness at whole site scale
newrow <- c(fours[1,1:3], 1, 16, 1,1)
newrow <- c(newrow, colSums(
  fours[fours$SubplotRes == 8, 8:ncol(fours)]
))
newrow[8:length(newrow)][
  which(newrow[8:length(newrow)] > 1)
  ] <- 1
names(newrow) <- colnames(fours)

# Binding new data to original dataset
fours <- rbind(fours, newrow)
comdata <- rbind(comdata, fours)

# Adding new columns
# Species richness value
comdata$Richness <- rowSums(comdata[8:ncol(comdata)])
# Subplot area in m2
comdata$SubplotArea <- comdata$SubplotRes ^ 2


# Plotting to check distribution
plot(comdata$Richness ~ log10(comdata$SubplotArea))
abline(lm(Richness ~ log10(SubplotArea), data=comdata))

# Writing new file
filename <- paste(unique(comdata$Site), 
                  "_Richness_Data.csv",
                  sep='')
write.csv(file=filename,x=comdata)
