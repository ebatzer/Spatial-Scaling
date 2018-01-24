# Calling packages and functions
library(dplyr);library(tidyr);library(plyr)
source("./Scripts/mergebycover.R")

#######################
# Reading in datasets #
#######################

# Sierra Foothill
com.matrix1 <- read.csv("./2017/SFRECSpring2017Edited.csv", stringsAsFactors = F)
# Hopland REC
com.matrix2 <- read.csv("./2017/HRECSpring2017Edited.csv", stringsAsFactors = F)
# McLaughlin REC
com.matrix3 <- read.csv("./2017/MclSpring2017Edited.csv", stringsAsFactors = F)

#################
# Data Cleaning #
#################

# Combining into master datasheet
# First adding SFREC and HREC
com.matrix <- merge(com.matrix1, com.matrix2, all = T)
# Then MCL
com.matrix <- merge(com.matrix, com.matrix3, all = T)
# Converting date to standard format
com.matrix$Date <- as.Date(com.matrix$Date, format = "%m/%d/%Y")

# Rename first column as index and set values from 1-nrow() for master
colnames(com.matrix)[1] <- "index"
com.matrix$index <- c(1:nrow(com.matrix))
# Turning all NA values into zeroes
com.matrix[is.na(com.matrix)] <- 0 

# Master ID spreadsheet is the first 9 columns:
com.ids <- com.matrix[,1:9]
# Species matrix is first column (index) and 10+ columns:
com.matrix <-  com.matrix[,-(2:9)]

####################################################################################
# Subsetting into individual smaller datasets that are then merged by maximum cover#
####################################################################################

# Looping over individual sites
for( sites in unique(com.ids$Site)){
  
  # Subsetting into temporary dataframes (com.subset, id.subset)
  com.subset <- com.matrix[com.ids$Site == sites,]
  id.subset <- com.ids[com.ids$Site == sites,]
  
  # Establishing the reference subset (subset 1) which will be used to add species to
  com.subset1 <- com.subset[id.subset$SiteVisit == sort(unique(id.subset$SiteVisit))[1],-1]
  id.subset1 <- id.subset[id.subset$SiteVisit == sort(unique(id.subset$SiteVisit))[1],-1]
  
  # Removing the rows that correspond to Date and SiteVisit (messes up mergebycover() function)
  id.subset1 <- id.subset1[,-c(2,8)]
  
  for( visits in sort(unique(id.subset$SiteVisit))[-1]){
    
    # Same as above, but looping over the other 2 dataframes
    com.subset2 <- com.subset[id.subset$SiteVisit == visits,-1]
    id.subset2 <- id.subset[id.subset$SiteVisit == visits,-1]
    id.subset2 <- id.subset2[,-c(2,8)]
    
    # Specifying series of columns to be ordered by (slowest-varying to fastest)
    id.order <- c("Site", "BlockNo", "PlotNo", "SubplotX", "SubplotY")

    # Merging together
    newmat <- merge_by_cover(com.matrix1 = com.subset1,
                             com.matrix2 = com.subset2,
                             com.id1 = id.subset1,
                             com.id2 = id.subset2,
                             id.order = id.order)
    
    # Setting our reference dataframe as the new matrix
    com.subset1 <- newmat
  }
  
    # If this is the first site, create a new dataframe
    if(sites == unique(com.ids$Site)[1]){
      data.merged <- cbind(id.subset1, com.subset1)
    # If not, merge with that dataframe using rbind.fill (better handles missing/different columns)
    }else{
      data.merged <- plyr::rbind.fill(list(data.merged, cbind(id.subset1, com.subset1)))
    }
    
    # Writing CSVs
    write.csv(x = data.merged, "./2017/alldatacombined2017.csv", row.names = F)
    
}
