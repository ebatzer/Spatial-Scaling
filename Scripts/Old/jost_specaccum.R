jost_specaccum <- function(com.data, # Community data
                           com.ids, # Community ids
                           n.perm, # Number of permutations, ignored for "spatial"
                           q.value, # Jost diversity level
                           spatial.columns, # Columns used to designate X and Y coordinates of data
                           accumulation.order # How are samples accumulated? Can be "random" or "spatial"
                           ){

  # Requires vegetarian to do Jost diversity calculations
  require(vegetarian)

  # Initializing output list
  output <- list()
  
  # Renames spatial columns to be X and Y
  names(com.ids)[names(com.ids) %in% spatial.columns] <- c("X", "Y")
    
  # Generate distance matrix
  plotdist <- as.matrix(dist(data.frame(X = com.ids$X, Y = com.ids$Y), diag = T, upper = T))
  
  sample.id <- c()
  divstore <- list()
  distance.means <- list()
  
  # Runs all combinations for spatial data
  if(accumulation.order == "spatial"){
    n.perm <- nrow(com.data)
    print("Ignoring n.perm argument with spatial accumulation")
    starts <- c(sample(1:nrow(com.data), nrow(com.data), replace = F))
  }

  # For a given number of permutations
  for(permno in 1:n.perm){
    
    # Add new elements for storage
    divstore <- c(divstore, NA)
    distance.means <- c(distance.means, NA)
    
    if(accumulation.order == "spatial"){
      startval <- starts[permno]
    }else{
      # Grab a row value from the dataset
      startval <- sample(1:nrow(com.data), 1)
    }

    # Store sample number
    sample.id[permno] <- startval

    # Pull out positions of rows based on proximity
    if(accumulation.order == "spatial"){
      plotorder <- order(plotdist[startval,], decreasing = F) 
    }else if(accumulation.order == "random"){
      plotorder <- c(startval, 
                     sample(c(1:nrow(com.data))[c(1:nrow(com.data)) != startval],
                          (nrow(com.data)) - 1))
    }else{
      stop("accumulation function must be specified as 'spatial' or 'random'")
    }
    
    # Store distance between samples
    distance.means[[permno]] <- plotdist[startval, plotorder]
    
    # Initialize storage vector
    divaccum <- c()

    # Generate distance matrix based on starting row value
    for(plot.iter in 1:length(plotorder)){
      divaccum[plot.iter] <- d(com.data[plotorder[1:plot.iter],], 
                               lev = "gamma",
                               q = q.value)
    }

    divstore[[permno]] <- divaccum

  }
  
  # Converting storage list to matrix
  divstore <- matrix(unlist(divstore), 
                     ncol = nrow(com.ids), 
                     byrow = TRUE)
  
  distance.means <- matrix(unlist(distance.means), 
                           ncol = nrow(com.ids), 
                           byrow = TRUE)
  
  output <- c(list(sample.id), list(divstore), list(distance.means))
  names(output) <- c("samples", "spec.accum", "dist.means")
  return(output)
  
}
