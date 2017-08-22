setwd("c:/Users/ebatz/Box Sync/Eviner lab shared/Evan/Research Projects/Spatial-Scaling/Spatial Scaling")
getwd()

library(tidyverse)
library(reshape2)
library(plyr)

dat <- read.csv("SFRECSpring2017Raw.csv", header = F, stringsAsFactors = F)

chunk.rows <- 17
tdata <- dat

nrow(dat)/chunk.rows

datamunger <- function(your.data, chunk.rows){
  
  tdata <- your.data
  
  master.chunk <- as.data.frame(tdata[2:chunk.rows,])
  colnames(master.chunk) <- as.character(c(tdata[1,]))
  colnames(master.chunk)[1] <- "Site"
  master.chunk[2:16,1:4] = master.chunk[1,1:4]
  master.chunk <- master.chunk[, names(master.chunk) != '']
  
  n.reps <- nrow(tdata)/chunk.rows
  
  for(sets in 1:(n.reps - 1)){
    
    tdata <- tdata[-(1:17),]
    
    munge.chunk <- as.data.frame(tdata[2:chunk.rows,])
    colnames(munge.chunk) <- as.character(c(tdata[1,]))
    munge.chunk[2:16,1:4] = munge.chunk[1,1:4]
    munge.chunk <- munge.chunk[, names(munge.chunk) != '']
  
    master.chunk <- join_all(list(as.tibble(master.chunk), as.tibble(munge.chunk)), type = "full")

  }
  
  return(master.chunk)
  
}

munged.data <- datamunger(dat, chunk.rows)
com.matrix <- munged.data[,8:ncol(munged.data)]
com.matrix <- com.matrix[,order(colnames(com.matrix))]
fulldata <- cbind(munged.data[,1:7], com.matrix)
write.csv(x = fulldata,
          "SFRECSpring2017Edited.csv")
