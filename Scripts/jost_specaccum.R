
jost_specaccum <- function(com.data, # Community data
                           com.ids, # Community ids
                           n.perm, # Number of permutations
                           q.value, # Jost diversity level
                           spatial.columns # Columns used to designate X and Y coordinates of data
                           ){

  require(vegetarian)

  # Initializing output list
  output <- list()
  
  # Renames spatial columns to be X and Y
  names(com.ids)[names(com.ids) %in% spatial.columns] <- c("X", "Y")
    
  # Generate distance matrix
  plotdist <- as.matrix(dist(data.frame(X = com.ids$X, Y = com.ids$Y), diag = T, upper = T))
  
  sample.id <- c()
  divstore <- list()

  # For a given number of permutations
  for(permno in 1:n.perm){
    
    divstore <- c(divstore, NA)
    
    # Grab a row value from the dataset
    startval <- sample(1:nrow(com.data), 1)
    
    # Store sample number
    sample.id[permno] <- startval
    
    # Pull out positions of rows based on proximity
    plotorder <- order(plotdist[startval,], decreasing = F)
    
    # Initialize storage vector
    divaccum <- c()
    plot.iter <- 200
    # Generate distance matrix based on starting row value
    for(plot.iter in 1:length(plotorder)){
      divaccum[plot.iter] <- d(com.data[plotorder[1:plot.iter],], 
                               lev = "gamma",
                               q = q.value)
    }

    divstore[[permno]] <- divaccum

  }
  
  # Converting storage list to matrix
  divstore <- matrix(unlist(divstore), ncol = nrow(com.ids), byrow = TRUE)
  
  output <- c(list(sample.id), list(divstore))
  names(output) <- c("samples", "spec.accum")
  return(output)
  
}

com.matrix <- read.csv("./2016/alldatacombined2016.csv", stringsAsFactors = F)[,-1]
com.ids <- read.csv("./2016/alldataidentification2016.csv", stringsAsFactors = F)
com.matrix$BriMin <- as.numeric(com.matrix$BriMin)
com.matrix[is.na(com.matrix)] <- 0
n.perm = 10
q.value = 0
spatial.columns = c("mid_x", "mid_y")

sites <- c("MCL", "HREC", "SFREC")
scales <- c(1, .5, .25)
blocks <- c(1:4)
spatcols <- c("mid_x", "mid_y")
n.perm = 99
storage <- list()
counter <- 1

for(site_name in sites){
  for(scaleval in scales){
    for(blockval in blocks){
      
      storage <- c(storage, NA)
      
      storage[[counter]] <- list(data.frame(site = site_name,
                                            scale = scaleval),
                                 jost_specaccum(
                                   com.data = com.matrix[
                                    com.ids$site_code == site_name & 
                                    com.ids$subplotres == scaleval &
                                    com.ids$block == blockval,],
                                   com.ids = com.ids[
                                    com.ids$site_code == site_name & 
                                    com.ids$subplotres == scaleval &
                                    com.ids$block == blockval,],
                                   n.perm = n.perm,
                                   q.value = q.value,
                                   spatial.columns = spatial.columns))
      
      counter <- counter + 1 
    }
  }
}



library(tidyr)
longdata <- list(NULL)
longdata
i <- 1

for( i in 1:length(storage)){
  runs <- storage[[i]][[2]]$spec.accum
  id <- storage[[i]][[1]]
  
  longdata[[1]] <- rbind(longdata[[1]],
                         gather(cbind(data.frame(id, run = c(1:nrow(runs)), runs)), 
         key = "samples", 
         value = "div",
         columns = -(1:3)))
  
}

longdata <- longdata[[1]]
longdata$samples <- as.numeric(gsub("X", "", longdata$samples))

library(ggplot2)

ggplot(aes(x = (samples * (scale ^ 2)),
           y = div,
           color = as.factor(scale)),
       data = longdata) + geom_smooth() + facet_wrap(~site)
