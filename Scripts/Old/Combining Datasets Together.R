# Calling packages

library(dplyr);library(tidyr);library(testthat)

source("./Scripts/mergebycover.R")

# Reading in datasets

# Late season community composition and species ID
late.com <- read.csv('2016/lateseason2016_communitymatrix.csv', 
                     header = T,
                     stringsAsFactors = F)[,-1]

late.ids <- read.csv('2016/lateseason2016_communityids.csv', 
                     header = T,
                     stringsAsFactors = F)

# Mid season community composition and species ID
mid.ids <- read.csv('2016/springdatacombined.csv', 
                    header = T,
                    stringsAsFactors = F)[,(3:9)]

mid.com <- read.csv('2016/springdatacombined.csv', 
                    header = T,
                    stringsAsFactors = F)

mid.com <- mid.com[,10:ncol(mid.com)]

# Reassigns cover scores using daubenmire bins
mid.com[,1][mid.com[,1] == 1] <- ((0+1)/2)
mid.com[,1][mid.com[,1] == 2] <- ((1+5)/2)
mid.com[,1][mid.com[,1] == 3] <- ((5+25)/2)
mid.com[,1][mid.com[,1] == 4] <- ((25+50)/2)
mid.com[,1][mid.com[,1] == 5] <- ((50+75)/2)
mid.com[,1][mid.com[,1] == 6] <- ((75+95)/2)
mid.com[,1][mid.com[,1] == 7] <- ((95+100)/2)

# Reassigns cover scores using daubenmire bins
for(colval in 1:ncol(late.com)){
  late.com[,colval][late.com[,colval] == 1] <- ((0+1)/2)
  late.com[,colval][late.com[,colval] == 2] <- ((1+5)/2)
  late.com[,colval][late.com[,colval] == 3] <- ((5+25)/2)
  late.com[,colval][late.com[,colval] == 4] <- ((25+50)/2)
  late.com[,colval][late.com[,colval] == 5] <- ((50+75)/2)
  late.com[,colval][late.com[,colval] == 6] <- ((75+95)/2)
  late.com[,colval][late.com[,colval] == 7] <- ((95+100)/2)
}

# Removing all rows with a subplot resolution less than 2 (no cover est. for large sizes)
late.com <- late.com[late.ids$SubplotRes < 2,]
late.ids <- late.ids[late.ids$SubplotRes < 2,]

# Checking that the number of rows in each dataset are the same
expect_true(length(unique(c(nrow(late.com), nrow(mid.com), nrow(late.ids), nrow(mid.ids)))) == 1)

# Cleaning column names
names(late.ids)[2] <- "Site" 
names(late.ids)[6] <- "SubplotNo"
names(late.ids)[7] <- "X"
names(late.ids)[8] <- "Y"
late.ids <- late.ids[,-1]
names(mid.ids)[2] <- "Block"
names(mid.ids)[3] <- "Plot"

# Checking that the same column names are present in each dataset
expect_true(unique(names(late.ids) %in% names(mid.ids)))

# Specifying series of columns to be ordered by (slowest-varying to fastest)
id.order <- c("Site", "Block", "Plot", "SubplotRes", "SubplotNo")

com.id1 <- cbind(index = c(1:nrow(late.ids)), late.ids)
com.id2 <- cbind(index = c(1:nrow(mid.ids)), mid.ids)
com.matrix1 <- cbind(index = c(1:nrow(late.com)), late.com)
com.matrix2 <- cbind(index = c(1:nrow(mid.com)), mid.com)


# Merging together
newmat <- merge_by_cover(com.matrix1 = com.matrix1[,-1],
                         com.matrix2 = com.matrix2[,-1],
                         com.id1 = com.id1[,-1],
                         com.id2 = com.id2[,-1],
                         id.order = id.order)

newmat <- cbind(index = c(1:nrow(newmat)), newmat)

# Adding in spatial data
spatdat <- read.csv("./2016/plot coordinates.csv")
head(spatdat)
names(spatdat)[2:5] <- c("Block", "Plot", "SubplotNo", "SubplotRes")
spatdat <- spatdat[,-1]

# Merging with ID dataframe
com.id1 <- merge(com.id1, spatdat, all.x = TRUE)
com.id1 <- com.id1[order(com.id1$index),]

# Writing CSVs
write.csv(x = newmat,
          "./2016/alldatacombined2016.csv", row.names = F)

write.csv(x = com.id1,
          "./2016/alldataidentification2016.csv", row.names = F)
