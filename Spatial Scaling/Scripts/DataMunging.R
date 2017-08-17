setwd("c:/Users/ebatz/Box Sync/Eviner lab shared/Evan/Research Projects/Spatial-Scaling/Spatial Scaling")
getwd()

library(tidyverse)
library(reshape2)
merge_all()
dat <- read.csv("MclSpring2017Raw.csv", header = F, stringsAsFactors = F)
View(dat)

chunk.rows <- 17

datamunger <- function(your.data, chunk.rows){
  
  tdata <- your.data
  
  master.chunk <- as.data.frame(tdata[2:chunk.rows,])
  colnames(master.chunk) <- as.character(c(tdata[1,]))
  
  n.reps <- nrow(tdata)/chunk.rows
  
  for(sets in 1:(n.reps - 1)){
    
    tdata <- tdata[-(1:17),]
    
    munge.chunk <- as.data.frame(tdata[2:chunk.rows,])
    colnames(munge.chunk) <- as.character(c(tdata[1,]))
    
    missingvals <- colnames(munge.chunk)[!colnames(munge.chunk) %in% colnames(master.chunk)]
    missingdata <- data.frame(matrix("NA", 
                      nrow = chunk.rows - 1, 
                      ncol = length(missingvals))
    )
    names(missingdata) <- missingvals
    master.chunk <- cbind(master.chunk, missingdata)
    
    master.chunk <- merge_all(list(master.chunk, munge.chunk))
    
  }

}
?rbind
tdata <- dat
View(master.chunk)
2176/17
