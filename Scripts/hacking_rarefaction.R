rarefaction_varq <- function (x, method, effort = NULL, coords = NULL, latlong = NULL, 
          dens_ratio = 1, extrapolate = FALSE, return_NA = FALSE, 
          quiet_mode = FALSE, q = 2) 
{
  if (!any(method %in% c("indiv", "samp", "spat"))) 
    stop("method must be \"indiv\", \"samp\", or \"spat\" for random individual, random sample, and spatial sample-based rarefaction, respectively")
  if (class(x) == "mob_in") {
    x_mob_in = x
    x = x_mob_in$comm
    if (is.null(latlong)) 
      latlong = x_mob_in$latlong
    else if (latlong != x_mob_in$latlong) 
      stop(paste("The \"latlong\" argument is set to", 
                 latlong, "but the value of x$latlong is", x_mob_in$latlong))
    if (is.null(coords)) 
      coords = x_mob_in$spat
  }
  if (method == "samp" | method == "spat") {
    if (is.null(dim(x))) 
      stop("For random or spatially explicit sample based rarefaction \"x\" must be a site x species matrix as the input")
    else {
      # x = (x > 0) * 1
      n = nrow(x)
      if (method == "samp") 
        x = colSums(x)
    }
  }
  if (method == "indiv") {
    if (!is.null(dim(x))) 
     x = colSums(x)
    n = sum(x)
  }
  if (is.null(effort)) 
    if (n == 0) 
      effort = 0
    else effort = 1:n
    if (any(effort > n)) {
      if (extrapolate & return_NA) 
        stop("It does not make sense to set \"extrapolate\" and \"return_NA\" to both be TRUE, see documentation")
      if (!quiet_mode) {
        warning_mess = paste("\"effort\" larger than total number of", 
                             ifelse(method == "indiv", "individuals", "samples"), 
                             "returning")
        if (extrapolate) 
          warning(paste(warning_mess, "extrapolated S using Chao1"))
        else if (return_NA) 
          warning(paste(warning_mess, "NA"))
        else warning(paste(warning_mess, "S"))
      }
    }
    else if (extrapolate) 
      if (!quiet_mode) 
        message("Richness was not extrapolated because effort less than or equal to the number of samples")
    if (method == "spat") {
      explicit_loop = matrix(0, n, n)
      if (is.null(latlong)) 
        stop("For spatial rarefaction the argument \"latlong\" must be set TRUE or FALSE")
      if (latlong) {
        pair_dist = sphere_dist(coords)
      }
      else {
        pair_dist = as.matrix(dist(coords))
      }
      for (i in 1:n) {
        dist_to_site = pair_dist[i, ]
        new_order = sample(1:n)
        dist_new = dist_to_site[new_order]
        new_order = new_order[order(dist_new)]
        new_order = c(i, new_order[new_order != i])
        comm_ordered = x[new_order, ]
        
        # New additions here
        abund = apply(comm_ordered, MARGIN = 2, cumsum)
        explicit_loop[, i] = calculate_alpha_diversity(as.matrix(abund), q = q)
      }
      out = apply(explicit_loop, 1, mean)[effort]
    }
    else {
      x = x[x > 0]
      S = length(x)
      if (dens_ratio == 1) {
        ldiv = lchoose(n, effort)
      }
      else {
        effort = effort[effort/dens_ratio <= n]
        ldiv = lgamma(n - effort/dens_ratio + 1) - lgamma(n + 
                                                            1)
      }
      p = matrix(0, sum(effort <= n), S)
      out = rep(NA, length(effort))
      S_ext = NULL
      for (i in seq_along(effort)) {
        if (effort[i] <= n) {
          if (dens_ratio == 1) {
            p[i, ] = ifelse(n - x < effort[i], 0, exp(lchoose(n - 
                                                                x, effort[i]) - ldiv[i]))
          }
          else {
            p[i, ] = ifelse(n - x < effort[i]/dens_ratio, 
                            0, exp(suppressWarnings(lgamma(n - x + 1)) - 
                                     suppressWarnings(lgamma(n - x - effort[i]/dens_ratio + 
                                                               1)) + ldiv[i]))
          }
        }
        else if (extrapolate) {
          f1 = sum(x == 1)
          f2 = sum(x == 2)
          f0_hat <- ifelse(f2 == 0, (n - 1)/n * f1 * (f1 - 
                                                        1)/2, (n - 1)/n * f1^2/2/f2)
          A = n * f0_hat/(n * f0_hat + f1)
          S_ext = c(S_ext, ifelse(f1 == 0, S, S + f0_hat * 
                                    (1 - A^(effort[i] - n))))
        }
        else if (return_NA) 
          S_ext = c(S_ext, NA)
        else S_ext = c(S_ext, S)
      }
      out = rep(NA, length(effort))
      out[effort <= n] = rowSums(1 - p)
      out[effort > n] = S_ext
    }
    names(out) = effort
    return(out)
}
