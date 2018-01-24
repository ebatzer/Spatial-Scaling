library(mobr)

master <- read.csv("./2017/alldatacombined2017.csv")
descriptions <- read.csv("./2017/SiteDescriptions.csv")
com.mat <- master[,8:ncol(master)] 
com.attr <- master[,1:7]

com.attr <- merge(com.attr, descriptions)

for(i in 2:ncol(com.mat)){
  for(j in 1:nrow(com.mat)){
      if(com.mat[j,i] == 1){com.mat[j,i] <- ((0+1)/2)}
      if(com.mat[j,i] == 2){com.mat[j,i] <- ((1+5)/2)}
      if(com.mat[j,i] == 3){com.mat[j,i] <- ((5+25)/2)}
      if(com.mat[j,i] == 4){com.mat[j,i] <- ((25+50)/2)}
      if(com.mat[j,i] == 5){com.mat[j,i] <- ((50+75)/2)}
      if(com.mat[j,i] == 6){com.mat[j,i] <- ((75+95)/2)}
      if(com.mat[j,i] == 7){com.mat[j,i] <- ((95+100)/2)}
      if(com.mat[j,i] == 0){com.mat[j,i] <- 0}
      
  }
}

sapply(com.mat, class)

# Creating storage for output
plot.rarefaction <- list()
plot.deltaS <- list()
counter <- 1

for(sites in unique(com.attr$Site)){
  for(blocks in unique(com.attr$BlockNo)){
    
    # Subsetting datasets by site and block
    mat.subset <- com.mat[com.attr$Site == sites & com.attr$BlockNo == blocks,]
    attr.subset <- com.attr[com.attr$Site == sites & com.attr$BlockNo == blocks,]
    
    # Correcting column names
    colnames(attr.subset)[5:6] <- c('x', 'y')
    
    # Making mob_in
    ss_mob_in = make_mob_in(comm = mat.subset, plot_attr = attr.subset)

    # Plotting and saving rarefaction
    pdf(paste("./MobrOutput/Rarefaction", sites,"_", blocks, ".pdf", sep = ""))
    samp.rarefaction <- plot_rarefaction(ss_mob_in, method = "samp", 'Fertilized', ref_group = 0)
    dev.off()

    # Saving rarefaction values in spreadsheet
    plot.rarefaction[counter] <- list(cbind(site = rep(sites, 2),
                                         block = rep(blocks, 2),
                                         plots = c(1,2),
                                         matrix(c(rarefaction(mat.subset[attr.subset$PlotNo == 1,], method = "samp"),
                                                  rarefaction(mat.subset[attr.subset$PlotNo == 2,], method = "samp")),
                                                nrow = 2, byrow = T)))
      
    # Gets delta_stats
    # ss_mob_out = get_delta_stats(ss_mob_in, 'Fertilized', ref_group=0,
    #                              type='discrete', log_scale=FALSE, n_perm=100,
    #                              tests = c("SAD", "agg"),
    #                              inds = 20)
    # 
    # plot(ss_mob_out, ref_group = "0", trt_group = "1")[3]
    # 
    # ss_mob_out$block <- blocks
    # ss_mob_out$site <- sites
    # plot.deltaS[counter][[1]] <- list(site = ss_mob_out$site,
    #      block = ss_mob_out$block,
    #      agg = ss_mob_out$agg)
    
    UF_agg <- jost_specaccum(
      com.data = mat.subset[attr.subset$Fertilized == 0,],
      com.ids = attr.subset[attr.subset$Fertilized == 0,],
      n.perm = 100,
      q.value = 1,
      spatial.columns = c("x", "y"),
      accumulation.order = "spatial")
    
    UF_rand <- jost_specaccum(
      com.data = mat.subset[attr.subset$Fertilized == 0,],
      com.ids = attr.subset[attr.subset$Fertilized == 0,],
      n.perm = 100,
      q.value = 1,
      spatial.columns = c("x", "y"),
      accumulation.order = "random")
    
    
    UF_dat <- cbind(data.frame(type = c("rand", "agg"), site = rep(attr.subset$Site[1], 2),
                               block = rep(attr.subset$BlockNo[1],2),  fert = rep(0,2)),
                      rbind(data.frame(UF_rand$spec.accum) %>% summarise_all(., .funs = mean),
                            data.frame(UF_agg$spec.accum) %>% summarise_all(., .funs = mean))) %>%
      gather(key = "plots",
             value = "diversity",
             -c(1:4))
    
    
    F_agg <- jost_specaccum(
      com.data = mat.subset[attr.subset$Fertilized == 1,],
      com.ids = attr.subset[attr.subset$Fertilized == 1,],
      n.perm = 100,
      q.value = 1,
      spatial.columns = c("x", "y"),
      accumulation.order = "spatial")
    
    F_rand <- jost_specaccum(
      com.data = mat.subset[attr.subset$Fertilized == 1,],
      com.ids = attr.subset[attr.subset$Fertilized == 1,],
      n.perm = 100,
      q.value = 1,
      spatial.columns = c("x", "y"),
      accumulation.order = "random")
    
    F_dat <- cbind(data.frame(type = c("rand", "agg"), site = rep(attr.subset$Site[1], 2),
                               block = rep(attr.subset$BlockNo[1],2),  fert = rep(1, 2)),
                    rbind(data.frame(F_rand$spec.accum) %>% summarise_all(., .funs = mean),
                          data.frame(F_agg$spec.accum) %>% summarise_all(., .funs = mean))) %>%
      gather(key = "plots",
             value = "diversity",
             -c(1:4))
    
    plot.deltaS[counter][[1]] <- list(rbind(UF_dat, F_dat))
    
    # Increasing counter value
    counter <- counter + 1
    
  }
}

plot.deltaS %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="i"), .)


merged.data.frame = Reduce(function(...) merge(..., all=T), plot.deltaS)

merged.data.frame$plots <- as.numeric(gsub("X", "", merged.data.frame$plots))


ggplot(aes(x = as.numeric(plots),
           y = as.numeric(diversity),
           color = factor(fert),
           lty = factor(type)),
       data = merged.data.frame[merged.data.frame$site == "MCL" &
                                  merged.data.frame$block == 2,]) + 
  geom_line() + 
theme(axis.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      plot.title = element_text(size = 24)) +
  xlab("Number of Plots") +
  ylab("Community Diversity") +
  ggtitle("Diversity Accumulation") + 
  theme(panel.background = element_rect(fill = "white"))

