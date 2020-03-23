# Testing for spatial effects

library(mobr); library(permute)

# Some helper functions for the actual permutation test function

# Each row in the matrix returned is one permutation (given as permuted indices)
# Allows a strata to constrain permutation set; assumes strata exists
# fixed_order: is permutation done for each plot or is the same permutation
# applied to all plots?
permutation_set_spatial <- function(nrow, nperm, strata, fixed_order = FALSE) {
  permcontrol <- permute::how(within = Within(constant = fixed_order), 
                               plots = Plots(strata = strata))
  permset <- permute::shuffleSet(nrow, nset = nperm, control = permcontrol)
  return(permset)
}

# Calculates rarefaction curves for all levels of strata and bundles them together
# Could very easily be hiding bugs but it seems to work?
rarefaction_curves <- function(species_abundance, coords, strata) {
  curves <- data.frame()
  for (strata_level in levels(strata)) {
    curves <- rbind(curves, mobr::rarefaction(species_abundance[strata == strata_level,], "spat", 
                                              coords = coords, latlong = FALSE))
  }
  names(curves) <- 1:ncol(curves)
  curves <- cbind(strata = levels(strata), curves)
  return(curves)
}

# Wrapper for mobr::rarefaction that handles permutation process
# This seems like it could be refactored a bit (currently exists so I can outsource apply)
# The logic is kind of awkward but it works, I think
permuted_rarefaction <- function(permute_info, species_abundance, coords, strata) {
  permute_order <- permute_info[-1]
  permute_iter <- permute_info[1]
  permuted_species_abundance <- species_abundance[permute_order, ]
  curves <- rarefaction_curves(species_abundance, coords, strata)
  curves <- cbind(permute_iter = permute_iter, curves)
  return(curves)
  # This can instead return a set of statistics?
}

# Permutation testing function
# Strata corresponds to one plot here (todo: rename?)
permute_spatial <- function(species_abundance, treatment, coords, strata = NULL, nperm = 999, test = 'two-sided') {
  
  # Preliminary setup steps
  # If there is no strata specified, create a strata column of 1
  if (is.null(strata)) {
    strata <- rep(1, nrow(species_abundance))
  }
  
  # Coerce treatment and strata to factors
  treatment <- as.factor(treatment)
  strata <- as.factor(strata)
  
  # TODO: Enforce one strata at each treatment level?
  
  # Calculate rarefaction curves, unpermuted
  curve0 <- rarefaction_curves(species_abundance, coords, strata)
  
  # Calculate rarefaction curves in permutatuions
  permset <- permutation_set_spatial(nrow(species_abundance), nperm, strata)
  permset <- cbind(1:nrow(permset), permset)
  curves <- apply(permset, 1, permuted_rarefaction, species_abundance = species_abundance, coords = coords, strata = strata)
  curves <- do.call(rbind, curves)
  # This is probably inefficient, since it requires storing each item of the list produced by apply
  # but, well, it works?

  # Final object to be returned
  rarefaction_test <- list(curve0 = curve0,
                           curves = curves,
                     nperm = nperm,
                     strata = strata)
  
  return(rarefaction_test)
}