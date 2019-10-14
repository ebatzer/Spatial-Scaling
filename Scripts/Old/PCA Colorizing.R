################################################################################
# Plotting Jost accumulation curves#############################################
################################################################################

# Calling Jost function
source("./Scripts/jost_specaccum.R")

# Calling libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in files
com.matrix1 <- read.csv("./2017/SFRECSpring2017Edited.csv", stringsAsFactors = F)
com.matrix2 <- read.csv("./2017/HRECSpring2017Edited.csv", stringsAsFactors = F)
com.matrix3 <- read.csv("./2017/MCLSpring2017Edited.csv", stringsAsFactors = F)
colnames(com.matrix3)[1:8] <- colnames(com.matrix2)[1:8]


com.matrix <- merge(com.matrix1, com.matrix2, all = T)
com.matrix <- merge(com.matrix, com.matrix3, all = T)
dates <- unique(com.matrix$Date)[10:18]
com.matrix <- com.matrix[com.matrix$Date %in% dates,]

com.matrix[is.na(com.matrix)] <- 0
com.ids <- com.matrix[,1:8]

com.matrix <- com.matrix[order(com.matrix$X),]
com.matrix <- com.matrix[order(com.ids$X),]
com.matrix <- com.matrix[,-(1:8)]
com.matrix[is.na(com.matrix$X),]

com.matrix <- com.matrix %>% select(-one_of("Hole", "Note", "Oak", "Wood"))

### Running PCA analysis on each dataset

com.subset <- com.matrix[com.ids$site_code == "HREC" &
                           com.ids$block == blockno,]

ir.pca <- prcomp(com.subset,
                 center = TRUE,
                 scale. = TRUE) 
