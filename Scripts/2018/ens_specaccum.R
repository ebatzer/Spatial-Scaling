# In the updated version, don't need to do randomized vs. spatial accumulation
# Instead, I'd like to get the mean output of spatial sample accumulation from each focal sample
# It's likely easier to:
  # Use the coordinates provided to produce a distance matrix
    # If ties exist, randomize
  # Order the dataset by the distance matrix
  # Produce a new dataset by using the cumsum() function s.t. each row represents a number of samples accounted for
  # Calculate a matrix of output
    # Each row is an iteration, each column is a value
  # An object containing: 
    # The mean of each column in the matrix
    # The matrix itself
# Simulations can then be performed by shuffling the community data and re-running spatial accumulation

# Also probably more efficient to reproduce a custom ENS function (ensCalc)

ensCalc <- function(com.matrix, q, sample.weight = 1){
  com.sums <- 0
  
  if(is.vector(com.matrix)){
    if( q == 1){
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums - sum(temp.matrix * log(temp.matrix))
      div <- exp(com.sums)
      
    }else{
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums + sum(temp.matrix ^ q)
      
      div <- com.sums ^ (1 / (1 - q))
      
    }
  }else{
    if( q == 1){
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums -(sample.weight[i] * sum(temp.matrix * log(temp.matrix)))
      }
      
      div <- exp(com.sums)
      
    }else{
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums + (sample.weight[i] ^ q) * (sum(temp.matrix ^ q))
      }
      
      div <- (com.sums / sum(sample.weight^q)) ^ (1 / (1 - q))
      
    }
  }
  
  return(div)
}

normalize_rows <- function(com.matrix){
  
  row.totals <- rowSums(com.matrix)
  
  com.matrix <- as.matrix(com.matrix)
  
  for(i in 1:nrow(com.matrix)){
    com.matrix[i, ] <- com.matrix[i, ] / row.totals[i]
  }
  
  return(com.matrix)
}


jost_specaccum_v2<- function(com.data, # Community data
                           com.ids, # Community ids
                           q.value, # Jost diversity level
                           spatial.columns, # Columns used to designate X and Y coordinates of data
                           order = "spatial"
){
  
  # Initializing output list
  output <- list()
  
  # Renames spatial columns to be X and Y
  names(com.ids)[names(com.ids) %in% spatial.columns] <- c("X", "Y")
  
  # Generate distance matrix
  plotdist <- as.matrix(dist(data.frame(X = com.ids$X, Y = com.ids$Y), diag = T, upper = T))
  
  # Initializing vectors for storage 
  meanAccum <- list()
  rawAccum <- list()
  
  if(order == "random"){
    com.data = com.data[sample(c(1:nrow(com.data)), nrow(com.data), replace = F)]
  }

  # For a given number of permutations
  for(sampIndex in 1:nrow(com.data)){
    
    # Pull out positions of rows based on proximity
    plotorder <- order(plotdist[sampIndex,], decreasing = F) 

    # Initialize storage vector
    divaccum <- c()
    
    # Perform cumSum on all columns
    comSummed <- apply(com.data[plotorder,], 2, cumsum)
    comSummed <- normalize_rows(comSummed)

    # Calculate ENS based on the ordered community dataset
    divaccum <- apply(comSummed, 1, ensCalc, q = q.value)
    
    # Store the vector of accumulated diversity
    rawAccum[[sampIndex]] <- divaccum
    
  }
  
  # Produce the matrix of all diversity values
  allAccum <- matrix(unlist(rawAccum), byrow = TRUE, ncol = nrow(com.data))
  
  # Calculate mean of all columns
  meanAccum <- apply(allAccum, 2, mean)
  
  # Return both the mean and all diversity value matrix
  output <- list(meanAccum, allAccum)
  
  return(output)
  
}

#' Jost diversity calculation
#'
#' Creates d_calc function that calculates Jost diversity for a dataframe
#' @keywords jost
#' @export
#' @examples
#' d_calc()

d_calc <- function(com.matrix, q, sample.weight){
  com.sums <- 0
  
  if(is.vector(com.matrix)){
    if( q == 1){
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums - sum(temp.matrix * log(temp.matrix))
      
      div <- exp(com.sums)
      
    }else{
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums + sum(temp.matrix ^ q)
      
      div <- (com.sums / sum(sample.weight^q)) ^ (1 / (1 - q))
      
    }
  }else{
    if( q == 1){
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums -(sample.weight[i] * sum(temp.matrix * log(temp.matrix)))
      }
      
      div <- exp(com.sums)
      
    }else{
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums + (sample.weight[i] ^ q) * (sum(temp.matrix ^ q))
      }
      
      div <- (com.sums / sum(sample.weight^q)) ^ (1 / (1 - q))
      
    }
  }
  
  
  return(div)
}
