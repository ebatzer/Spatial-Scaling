
library(dplyr);library(tidyr)

# Calling species matrix function
source("../NutNet/NutNetTemporalScaling/scripts/genspecmat.R")

# Loading CSVs
hrec <- read.csv("./2016/hrec_lateseason2016.csv", stringsAsFactors = F)
mcl <- read.csv("./2016/mcl_lateseason2016.csv", stringsAsFactors = F)
sfrec <- read.csv("./2016/sfrec_lateseason2016.csv", stringsAsFactors = F)

# Converting to long format and combining
h_long <- hrec %>% gather(key = "species", value = "cover",
                -c(1:7))
m_long <- mcl %>% gather(key = "species", value = "cover",
                          -c(1:7))
s_long <- sfrec %>% gather(key = "species", value = "cover",
                          -c(1:7))

names(m_long)[5] <- "Subplot"
sites_combined <- rbind(h_long, m_long, s_long)

# Cleaning dataset
sites_combined$cover[sites_combined$cover == ""] <- 0
sites_combined$cover[is.na(sites_combined$cover)] <- 0
sites_combined$cover[sites_combined$cover == "X"] <- 1
sites_combined$cover <- as.numeric(sites_combined$cover)

# Generating species matrix
specmat <- genSpecMat(sites_combined,
           c(1:7),
           "species",
           "cover")

# Writing CSVs
write.csv("2016/lateseason2016_communitymatrix.csv",
          x = specmat$community_matrix)

write.csv("2016/lateseason2016_communityids.csv",
          x = specmat$sample_ids)
