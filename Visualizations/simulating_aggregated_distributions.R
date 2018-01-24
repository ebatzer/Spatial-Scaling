#devtools::install_github("MoBiodiv/mobsim", build_vignettes = TRUE)
# library(devtools)
# install.packages("Rcpp")
# install_github("MoBiodiv/mobsim")
# 
library(mobsim)
library(ggplot2)

comm_rand <- sim_thomas_community(s_pool = 4, n_sim = 200, mother_points = 1, cluster_points = 10, sigma = 1)
comm_agg <- sim_thomas_community(s_pool = 6, n_sim = 200, sigma = 0.25, mother_points = 1, cluster_points = 10)

agg_pts <- cbind(type = rep("agg", nrow(comm_agg$census)), comm_agg$census)
rand_pts <- cbind(type = rep("rand", nrow(comm_rand$census)), comm_rand$census)

spec_pts <- rbind(agg_pts, 
      rand_pts)

xvals <- seq(.10, .9, by = .2)
yvals <- seq(.10, .9, by = .2)
tile_pts <- data.frame( x = rep(xvals, length(xvals)), 
                        y = sort(rep(yvals, length(yvals))))
width = .1

ggplot(aes(x = x,
           y = y,
           color = species),
       data = spec_pts) +
  geom_tile(aes(color = NULL), data = tile_pts,
            width = width * 2,
            height = width * 2,
            fill = "white",
            color = "black",
            lwd = 1
           ) +   
  geom_point(color = 'black', size = 6) + 
  geom_point(size = 5) + 
  theme(panel.background = element_rect(fill = "white")) + 
  facet_wrap(~type) +
  guides(color = "none") + 
  scale_colour_brewer(palette = "Set1")

tile_pts$xhigh <- tile_pts$x + width
tile_pts$xlow <-tile_pts$x - width
tile_pts$yhigh <- tile_pts$y + width
tile_pts$ylow <-tile_pts$y - width

library(tidyr); library(dplyr); library(tidyverse)
install.packages("tidyselect")

agg_sum <- data.frame(species1 = NA,
                      species2 = NA,
                      species3 = NA,
                      species4 = NA,
                      species5 = NA,
                      species6 = NA)

ran_sum <- agg_sum

for(i in 1:nrow(tile_pts)){
  agg_data <- spec_pts %>% filter(type == "agg" &
                        x > tile_pts$xlow[i] &
                        x < tile_pts$xhigh[i] &
                        y > tile_pts$ylow[i] &
                        y < tile_pts$yhigh[i])
  
  if(nrow(agg_data) > 1){
    agg_sum <- bind_rows(agg_sum, agg_data %>% 
                           group_by(species) %>% summarise(count = n()) %>% spread(species, count))
  }else{
    agg_sum <- bind_rows(agg_sum, data.frame(species1 = NA,
                                             species2 = NA,
                                             species3 = NA,
                                             species4 = NA,
                                             species5 = NA,
                                             species6 = NA))
  }

  rand_data <- spec_pts %>% filter(type == "rand" &
                                    x > tile_pts$xlow[i] &
                                    x < tile_pts$xhigh[i] &
                                    y > tile_pts$ylow[i] &
                                    y < tile_pts$yhigh[i])
  
  if(nrow(rand_data) > 1){
    ran_sum <- bind_rows(ran_sum, rand_data %>% 
                           group_by(species) %>% summarise(count = n()) %>% spread(species, count))
  }else{
    ran_sum <- bind_rows(ran_sum, data.frame(species1 = NA,
                                             species2 = NA,
                                             species3 = NA,
                                             species4 = NA,
                                             species5 = NA,
                                             species6 = NA))
  }
  
 
  
}

agg_com <- cbind(tile_pts[,1:2], agg_sum[-1,])
rand_com <- cbind(tile_pts[,1:2], ran_sum[-1,])


source("./Scripts/jost_specaccum.R")

agg_com[is.na(agg_com)] <- 0

agg_output <- jost_specaccum(
  com.data = agg_com[,3:6],
  com.ids = agg_com[,1:2],
  n.perm = n.perm,
  q.value = 1,
  spatial.columns = c("x", "y"),
  accumulation.order = "spatial")

agg_output_rand <- jost_specaccum(
  com.data = agg_com[,3:6],
  com.ids = agg_com[,1:2],
  n.perm = 999,
  q.value = 1,
  spatial.columns = c("x", "y"),
  accumulation.order = "random")


rand_com[is.na(rand_com)] <- 0

rand_output <- jost_specaccum(
  com.data = rand_com[,3:6],
  com.ids = rand_com[,1:2],
  n.perm = 999,
  q.value = 1,
  spatial.columns = c("x", "y"),
  accumulation.order = "spatial")

rand_output_rand <- jost_specaccum(
  com.data = rand_com[,3:6],
  com.ids = rand_com[,1:2],
  n.perm = 999,
  q.value = 1,
  spatial.columns = c("x", "y"),
  accumulation.order = "random")

curvedat <- cbind(type = c("rand", "agg"),
      rbind(data.frame(rand_output$spec.accum) %>% summarise_all(., .funs = mean),
      data.frame(agg_output$spec.accum) %>% summarise_all(., .funs = mean))) %>%
  gather(key = "plots",
         value = "diversity",
         -c(1))
curvedat_rand <- cbind(type = c("rand", "agg"),
                  rbind(data.frame(rand_output_rand$spec.accum) %>% summarise_all(., .funs = mean),
                        data.frame(agg_output_rand$spec.accum) %>% summarise_all(., .funs = mean))) %>%
  gather(key = "plots",
         value = "diversity",
         -c(1))

curvedat$plots <- as.numeric(gsub("X", "", curvedat$plots))
curvedat_rand$plots <- as.numeric(gsub("X", "", curvedat$plots))

ggplot(aes(x = plots,
           y = diversity,
           color = type),
       data = curvedat) + 
  stat_smooth(span = 1.35,
              se = FALSE) + 
  stat_smooth(span = 1.2,
              se = FALSE,
              lty = 2,
              data = curvedat_rand) +
  # # Plotting NLS regression line
  # stat_smooth(method = "nls",
  #            formula = y ~ a*x^b,
  #            method.args = list(start = c(a = 1,
  #                                         b = 1)),
  #            se = FALSE,
  #            lwd = 2) +
  # stat_smooth(method = "nls",
  #             formula = y ~ a*x^b,
  #             method.args = list(start = c(a = 1,
  #                                          b = 1)),
  #             se = FALSE,
  #             lwd = 2,
  #             data = curvedat_rand,
  #             lty = 2, fullrange = T) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24)) +
  xlab("Number of Plots") +
  ylab("Community Diversity") +
  ggtitle("Diversity Accumulation") + 
  theme(panel.background = element_rect(fill = "white"))
