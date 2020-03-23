# performs the actual arithmetic for diversity metric calculation
calculate_metric <- function(p_i, q) {
  
  stopifnot(q >= 0) # some bounds checking to be safe
  
  p_i <- p_i[p_i != 0] # because 0^0 is not 0, not removing 0 breaks q = 0
  
  if (q == 1) {
    return(exp(-sum(p_i * log(p_i))))
    
  } else {
    return((sum(p_i^q))^(1/(1-q)))
  }
}

# helper function that takes a row and calculates diversity metric
# alpha diversity is this function applied to each observation
# gamma diversity is this function applied to the aggregated observations
calculate_diversity_row <- function(a_row, q) {
  total_sum <- sum(a_row)
  p_i <- sapply(a_row, sum)/total_sum
  
  return(calculate_metric(p_i, q))
}

calculate_alpha_diversity <- function(species_abundance, q) {
  
  # Check for all numeric values
  stopifnot(all(apply(species_abundance, 2, is.numeric)))
  
  return(apply(species_abundance, 1, calculate_diversity_row, q = q))
}

calculate_gamma_diversity <- function(species_abundance, q) {
  
  # Check for all numeric values
  stopifnot(all(apply(species_abundance, 2, is.numeric)))
  
  return(calculate_diversity_row(colSums(species_abundance), q))
}

# forces recalculation of alpha and gamma - 
# perhaps passing alpha and gamma as args is better?
calculate_beta_diversity <- function(species_abundance, q){
  
  # Check for all numeric values
  stopifnot(all(apply(species_abundance, 2, is.numeric)))
  
  alpha <- calculate_alpha_diversity(species_abundance, q)
  gamma <- calculate_gamma_diversity(species_abundance, q)
  
  return(gamma/alpha)
}