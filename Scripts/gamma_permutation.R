# Testing differences in gamma diversity

library(vegan); library(permute)

# Some helper functions for the actual permutation test function

# Each row in the matrix returned is one permutation (given as permuted indices)
# Allows a strata to constrain permutation set; assumes strata exists
permutation_set <- function(nrow, nperm, strata) {
  permcontrol <- permute::how(plots = Plots(strata = strata))
  permset <- permute::shuffleSet(nrow, nset = nperm, control = permcontrol)
  return(permset)
}

# Calculates test statistic; assumes only two treatments, assumes strata exists
gamma_test_statistic <- function(species_abundance, treatment, q, strata) {
  means_by_treatment_strata <- by(species_abundance, INDICES = list(treatment, strata), 
                                  FUN = calculate_gamma_diversity, q = q)
  t <- mean(means_by_treatment_strata[1, ]) - mean(means_by_treatment_strata[2, ])
  return(t)
}

# Wrapper for gamma_test_statistic that handles permutation process
# This seems like it could be refactored a bit (currently exists so I can outsource apply)
permuted_gamma_test_statistic <- function(permute_order, species_abundance, treatment, q, strata) {
  permuted_species_abundance <- species_abundance[permute_order, ]
  t <- gamma_test_statistic(permuted_species_abundance, treatment, q, strata)
  return(t)
}

# Permutation testing function
# Pretty easy to modify as a shell for other test statistics
permute_gamma <- function(species_abundance, treatment, q, strata = NULL, nperm = 999, test = 'two-sided') {
  
  # Preliminary setup steps
  # If there is no strata specified, create a strata column of 1
  if (is.null(strata)) {
    strata <- rep(1, nrow(species_abundance))
  }
  
  # Coerce treatment and strata to factors
  treatment <- as.factor(treatment)
  strata <- as.factor(strata)
  
  # Calculate test statistic, unpermuted
  t0 <- gamma_test_statistic(species_abundance, treatment, q, strata)
  
  # Calculate test statistic in permutatuions
  permset <- permutation_set(nrow(species_abundance), nperm, strata)
  t <- apply(permset, 1, permuted_gamma_test_statistic, species_abundance = species_abundance, 
             treatment = treatment, q = q, strata = strata)
  
  # Calculate p-value of test
  if (test == 'two-sided') {
    p = sum(abs(t) > abs(t0)) / (nperm + 1)
  } else if (test == 'greater') {
    p = sum(t > t0) / (nperm + 1)
  } else if (test == 'less') {
    p = sum(t < t0) / (nperm + 1)
  } else {
    stop('test" must be one of "two-sided", "greater", or "less"')
  }
  
  # Final object to be returned
  gamma_test <- list(t0 = t0,
                     t = t,
                     p = p,
                     test = test,
                     nperm = nperm,
                     q = q,
                     strata = strata)
  
  return(gamma_test)
}
