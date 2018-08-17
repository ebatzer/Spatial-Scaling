# Permutation significance testing
# Requires the following elements:
# Dataset to be tested
# ID columns of the dataset
# Factor column that denotes factor pair to be tested
# Q-value of biodiversity metric
# Number of permutations


div.permtest <- function(
  comdat, # Community data
  idcols, # ID columns of community data
  testcol, # Groups to test between
  qval, # Q-value
  n.perm, # Number of permutations (>1000 recommended)
  tails = "two", # Number of tails (can be one or two, two is default)
  direction = NA # Direction of the test, if one tailed
  ){
  
  # Sets label factor vector
  lab <- as.factor(comdat[,idcols][,colnames(comdat[,idcols]) == testcol])
  
  # Calculates alpha, beta, and gamma diversity values for each group
  grp.1.val <- unlist(jost_d(comdat[lab == levels(lab)[1],-idcols], 
                             q = qval, boot = FALSE)@summary[[1]])[1:3]
  grp.2.val <- unlist(jost_d(comdat[lab == levels(lab)[2],-idcols], 
                             q = qval, boot = FALSE)@summary[[1]])[1:3]
  
  # Establishes difference in values
  obs.diff <- grp.1.val - grp.2.val
  
  # Creating storage list for permutation test output
  perm.output <- list()
  
  for( perm in 1:n.perm){
    
    # Reshuffle label column at random
    permlab <- base::sample(lab, length(lab), replace = F)

    # Recalculates value based on permutation
    perm.1.val <- unlist(jost_d(comdat[permlab == levels(permlab)[1],-idcols], 
                                q = qval, boot = FALSE)@summary[[1]])[1:3]
    perm.2.val <- unlist(jost_d(comdat[permlab == levels(permlab)[2],-idcols], 
                                q = qval, boot = FALSE)@summary[[1]])[1:3]
    perm.diff <- perm.1.val - perm.2.val
    
    # Stores difference in value
    perm.output[[perm]] <- perm.diff[1:3]
    
  }
  
  # Unlists perm.output and creates dataframe
  sim.vals <- matrix(unlist(perm.output), ncol = 3, byrow = T)
  colnames(sim.vals) <- c("alpha", "beta", "gamma")
  
  # Significance testing
  div.signif <- c()
  
  for(divlevel in 1:3){
    if(tails == "two"){
      div.signif[divlevel] <-  (sum(abs(sim.vals[,divlevel]) >= abs(obs.diff[divlevel])) + 1) / (n.perm + 1) 
    }else if(tails == "one"){
      if(direction == "postiive"){
        div.signif[divlevel] <-  sum(sim.vals[,divlevel] >= obs.diff[divlevel]) / n.perm 
      }else if(direction == "negative"){
        div.signif[divlevel] <- sum(sim.vals[,divlevel] <= obs.diff[divlevel]) / n.perm 
      }else{
        stop("'direction' must be 'positive' or 'negative")
      }
    }else{
      stop("'tails' value must be 'one' or 'two'")
    }
  }
  
  # Assembling output dataframe
  fnctn.output <- list(obs.diff = list(obs.diff[1:3],
                                       div.signif),
                       obs.vals = list(grp.1.val,
                                       grp.2.val),
                       sim.vals = sim.vals)
  names(fnctn.output$obs.diff) = c("vals", "signif")
  names(fnctn.output$obs.vals) = c(levels(lab)[1], levels(lab)[2])
  
  return(fnctn.output)
  
}
