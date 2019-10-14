setwd("~/Research Projects/Spatial Scaling")

mcl <- read.csv("SFRECSpring2016Corrected.csv", stringsAsFactors = F)

head(mcl)

# Should check if diversity at larger scales is always greater than or equal to smaller scale richness values

# To Do:

# Polygon with 4 sets of coordinates

counter <- 1
sitevec <- c() # Site values
plotvec <- c() # Plot values
rowvec <- c() # Row values
scalevec <- c() # Subplot resolution
xframe <- matrix(ncol=4, nrow=nrow(mcl)) # Dataframe to store real X coordinates
yframe <- matrix(ncol=4, nrow=nrow(mcl)) # Dataframe to store real Y coordinates

for( i in rownames(mcl)){
  tempdat <- mcl[rownames(mcl) == i,]
  
  if( tempdat$SubplotRes >= 2){
    sitevec[counter] <- tempdat$SiteNo
    plotvec[counter] <- tempdat$PlotNo
    rowvec[counter] <- as.numeric(i)
    scalevec[counter] <- tempdat$SubplotRes
    xframe[counter,] <- c((tempdat$SubplotRes * tempdat$X) - tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) - tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X),
                          (tempdat$SubplotRes * tempdat$X))
    
    yframe[counter,] <- c((tempdat$SubplotRes * tempdat$Y) - tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$Y),
                          (tempdat$SubplotRes * tempdat$Y),
                          (tempdat$SubplotRes * tempdat$Y) - tempdat$SubplotRes)
  }
  
  if( tempdat$SubplotRes == 1){
    
    offsetX <-xframe[ 
               as.numeric(
                  rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                  mcl$PlotNo == tempdat$PlotNo &
                  mcl$SubplotRes == 2,])
                ),1
              ]
    offsetY <-yframe[ 
      as.numeric(
        rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                       mcl$PlotNo == tempdat$PlotNo &
                       mcl$SubplotRes == 2,])
      ),1
      ]
                  
    sitevec[counter] <- tempdat$SiteNo
    plotvec[counter] <- tempdat$PlotNo
    rowvec[counter] <- as.numeric(i)
    scalevec[counter] <- tempdat$SubplotRes
    xframe[counter,] <- c((tempdat$SubplotRes * tempdat$X) + offsetX - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX ,
                          (tempdat$SubplotRes * tempdat$X) + offsetX )
    
    yframe[counter,] <- c((tempdat$SubplotRes * tempdat$Y) + offsetY  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY - 
                            tempdat$SubplotRes)
  }
  
  if( tempdat$SubplotRes == .5){
    
    offsetX <-xframe[ 
      as.numeric(
        rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                       mcl$PlotNo == tempdat$PlotNo &
                       mcl$SubplotRes == 1,])[
                         if(tempdat$SubplotNo <= 2){
                           1
                         }else{
                           2
                         }])
        ,1
      ]
    offsetY <-yframe[ 
      as.numeric(
        rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                       mcl$PlotNo == tempdat$PlotNo &
                       mcl$SubplotRes == 1,])[
                         if(tempdat$SubplotNo <= 2){
                           1
                         }else{
                           2
                         }])
      ,1
      ]
    
    sitevec[counter] <- tempdat$SiteNo
    plotvec[counter] <- tempdat$PlotNo
    rowvec[counter] <- as.numeric(i)
    scalevec[counter] <- tempdat$SubplotRes
    xframe[counter,] <- c((tempdat$SubplotRes * tempdat$X) + offsetX - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX ,
                          (tempdat$SubplotRes * tempdat$X) + offsetX )
    
    yframe[counter,] <- c((tempdat$SubplotRes * tempdat$Y) + offsetY  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY - 
                            tempdat$SubplotRes)
  }
  
  if( tempdat$SubplotRes == .25){
    
    offsetX <-xframe[ 
      as.numeric(
        rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                       mcl$PlotNo == tempdat$PlotNo &
                       mcl$SubplotRes == .5,])[
                         if(tempdat$SubplotNo %in% c(1,2)){
                           1
                         }else if(tempdat$SubplotNo %in% c(3,4)){
                           2
                         }else if(tempdat$SubplotNo %in% c(5,6)){
                           3
                         }else if(tempdat$SubplotNo %in% c(7,8)){
                           4
                         }])
      ,1
      ]
    offsetY <-yframe[ 
      as.numeric(
        rownames(mcl[mcl$SiteNo == tempdat$SiteNo &
                       mcl$PlotNo == tempdat$PlotNo &
                       mcl$SubplotRes == .5,])[
                         if(tempdat$SubplotNo %in% c(1,2)){
                           1
                         }else if(tempdat$SubplotNo %in% c(3,4)){
                           2
                         }else if(tempdat$SubplotNo %in% c(5,6)){
                           3
                         }else if(tempdat$SubplotNo %in% c(7,8)){
                           4
                         }])
      ,1
      ]
    
    sitevec[counter] <- tempdat$SiteNo
    plotvec[counter] <- tempdat$PlotNo
    rowvec[counter] <- as.numeric(i)
    scalevec[counter] <- tempdat$SubplotRes
    xframe[counter,] <- c((tempdat$SubplotRes * tempdat$X) + offsetX - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$X) + offsetX ,
                          (tempdat$SubplotRes * tempdat$X) + offsetX )
    
    yframe[counter,] <- c((tempdat$SubplotRes * tempdat$Y) + offsetY  - 
                            tempdat$SubplotRes,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY,
                          (tempdat$SubplotRes * tempdat$Y) + offsetY - 
                            tempdat$SubplotRes)
  }
  counter <- counter + 1
}

coordslist <- list(sitevec, plotvec, rowvec, scalevec, xframe, yframe)
names(coordslist) <- c("site", "plot", "row", "scale", "x", "y")

coordslist$site[coordslist$site == 1]

par(mfrow=c(2,2))
for( sites in 1:4){
  plot.new()
  plot.window(xlim=c(0,8), ylim=c(0,8))
  axis(side=1)
  for( i in 1:sum(coordslist$site == sites)){
    polygon(x=coordslist$x[coordslist$site == sites,][i,],
            y=coordslist$y[coordslist$site == sites,][i,],
            density=(4/(coordslist$scale[coordslist$site == sites][i]))^2)  
  }
}

