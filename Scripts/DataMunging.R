# Munging datasets:

################################################################################

library(dplyr)
library(testthat)
library(tidyr)
source("scripts/mergebycover.R")

################################################################################

siteiter <- 0
merged_dat <- data.frame(NA)

for(sitename in c("MCL", "SFREC", "HREC")){
  
  # All data
  rawdat <- read.csv(paste("2018/", sitename, "_peak_2018.csv", sep = ""),
                     header = FALSE,
                     stringsAsFactors = FALSE)
  
  # Initializing dataframe
  newdat <- data.frame("NA")
  iter <- 0
  
  # While the new dataframe is not full
  while(nrow(newdat) < 512){
    
    # Segment the whole dataframe into a chunk containing 17 rows
    dat_chunk <- rawdat[c(c(1:17) + (17 * iter)),]
    
    # Turn the first row into a header
    names(dat_chunk) <- dat_chunk[1,]
    dat_chunk <- dat_chunk[-1,]
    rownames(dat_chunk) <- NULL
    
    dat_chunk <- dat_chunk[,colnames(dat_chunk) != ""]
    
    # If the starting index is equal to 0
    if(iter == 0){
      newdat <- dat_chunk
    }else{
      newdat <- bind_rows(newdat, dat_chunk)
    }
    
    iter <- iter + 1
    
  }
  
  newdat[is.na(newdat)] <- 0
  
  # Error checking:
  
  # Correct number of 
  expect_equal(nrow(newdat), 512)
  
  # Equal distribution across all datasets?
  newdat %>% group_by(SiteName, BlockNo, PlotNo) %>%
    summarise(obs = n())
  
  if(siteiter == 0){
    merged_dat <- newdat
  }else{
    merged_dat <- bind_rows(merged_dat, newdat)
  }
  
  siteiter <- siteiter + 1
  
  merged_dat[is.na(merged_dat)] <- 0
  
}

colorder <- c(colnames(merged_dat)[1:6], 
              sort(colnames(merged_dat)[7:length(colnames(merged_dat))]))

peak_dat <- merged_dat %>% select(colorder)

peak_dat[peak_dat == 0] <- ""

################################################################################
# Cleaning up column labels

col_labs <- read.csv("2018/ColumnLabels.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE)

clean_dat <- peak_dat[,1:6]

col_labs <- col_labs[unlist(lapply(col_labs[,1], FUN=function(x){x %in% colnames(peak_dat)})),]


for(colname in unique(col_labs$NewVal)){
  
  oldcols = col_labs$ColVal[col_labs$NewVal == colname]
    
  toadd = peak_dat %>% 
    select(oldcols) %>% unite(new, sep = "")
    
  names(toadd) = tolower(colname)
    
  clean_dat <- clean_dat %>% bind_cols(toadd)

  
}

peak_merged <- clean_dat

write.csv(x = clean_dat, "2018/merged_data_2018_peak.csv")

################################################################################

siteiter <- 0
merged_dat <- data.frame(NA)

for(sitename in c("MCL", "SFREC", "HREC")){
  
  # All data
  rawdat <- read.csv(paste("2018/", sitename, "_spring_2018.csv", sep = ""),
                     header = FALSE,
                     stringsAsFactors = FALSE)
  
  # Initializing dataframe
  newdat <- data.frame("NA")
  iter <- 0
  
  # While the new dataframe is not full
  while(nrow(newdat) < 512){
    
    # Segment the whole dataframe into a chunk containing 17 rows
    dat_chunk <- rawdat[c(c(1:17) + (17 * iter)),]
    
    # Turn the first row into a header
    names(dat_chunk) <- dat_chunk[1,]
    dat_chunk <- dat_chunk[-1,]
    rownames(dat_chunk) <- NULL
    
    dat_chunk <- dat_chunk[,colnames(dat_chunk) != ""]
    
    # If the starting index is equal to 0
    if(iter == 0){
      newdat <- dat_chunk
    }else{
      newdat <- bind_rows(newdat, dat_chunk)
    }
    
    iter <- iter + 1
    
  }
  
  newdat[is.na(newdat)] <- 0
  
  # Error checking:
  
  # Correct number of 
  expect_equal(nrow(newdat), 512)
  
  # Equal distribution across all datasets?
  newdat %>% group_by(SiteName, BlockNo, PlotNo) %>%
    summarise(obs = n())
  
  if(siteiter == 0){
    merged_dat <- newdat
  }else{
    merged_dat <- bind_rows(merged_dat, newdat)
  }
  
  siteiter <- siteiter + 1
  
  merged_dat[is.na(merged_dat)] <- 0
  
}

colorder <- c(colnames(merged_dat)[1:6], 
              sort(colnames(merged_dat)[7:length(colnames(merged_dat))]))

spring_dat <- merged_dat %>% select(colorder)

spring_dat[spring_dat == 0] <- ""

################################################################################

# Cleaning up column labels

col_labs <- read.csv("2018/ColumnLabels.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE)

clean_dat <- spring_dat[,1:6]

col_labs <- col_labs[unlist(lapply(col_labs[,1], FUN=function(x){x %in% colnames(spring_dat)})),]

for(colname in unique(col_labs$NewVal)){
  
  oldcols = col_labs$ColVal[col_labs$NewVal == colname]
  
  toadd = spring_dat %>% 
    select(oldcols) %>% unite(new, sep = "")
  
  names(toadd) = tolower(colname)
    
  clean_dat <- clean_dat %>% bind_cols(toadd)
  

}

spring_merged <- clean_dat

write.csv(x = spring_merged, "2018/merged_data_2018_spring.csv")

################################################################################
# Merging by cover (Only taking highest cover per sampling event)

id.order <- c("SiteName", "BlockNo", "PlotNo", "Subplot No", "Subplot X", "Subplot Y")

final_merged <- merge_by_cover(peak_merged[,7:ncol(peak_merged)], 
               spring_merged[,7:ncol(spring_merged)],
               peak_merged[,1:6],
               spring_merged[,1:6],
               id.order = id.order)

colorder <- c(colnames(final_merged)[1:6], 
              sort(colnames(final_merged)[7:length(colnames(final_merged))]))

final_merged <- final_merged %>% select(colorder)

################################################################################
# Finalized Version
write.csv(x = final_merged, "2018/merged_data_2018_final.csv")
################################################################################
