source("Code/diversity_metrics.R")
library(vegan)
data(dune)

# test alpha diversity against vegan equivalents
vegan::specnumber(dune) == calculate_alpha_diversity(dune, 0)
vegan::diversity(dune) == log(calculate_alpha_diversity(dune, 1))
vegan::diversity(dune, index = "invsimpson") == calculate_alpha_diversity(dune, 2)

# test gamma diversity against vegan using colSums
vegan::specnumber(colSums(dune)) == calculate_gamma_diversity(dune,0)
vegan::diversity(colSums(dune)) == log(calculate_gamma_diversity(dune, 1))
vegan::diversity(colSums(dune), index = "invsimpson") == 
  calculate_gamma_diversity(dune, 2)

# test beta diversity 
mean(vegan::specnumber(dune) / vegan::specnumber(colSums(dune))) == calculate_beta_diversity(dune,0)
mean(exp(vegan::diversity(dune)) / exp(vegan::diversity(colSums(dune)))) == calculate_beta_diversity(dune, 1)
mean(vegan::diversity(dune, index = "invsimpson") / vegan::diversity(colSums(dune), index = "invsimpson")) == calculate_beta_diversity(dune, 2)
