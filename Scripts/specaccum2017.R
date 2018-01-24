################################################################################
# Plotting Jost accumulation curves#############################################
################################################################################

# Calling Jost function
source("./Scripts/jost_specaccum.R")

# Calling libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in files
com.matrix1 <- read.csv("./2017/SFRECSpring2017Edited.csv", stringsAsFactors = F)
com.matrix2 <- read.csv("./2017/HRECSpring2017Edited.csv", stringsAsFactors = F)
com.matrix3 <- read.csv("./2017/MCLSpring2017Edited.csv", stringsAsFactors = F)
colnames(com.matrix3)[1:8] <- colnames(com.matrix2)[1:8]


com.matrix <- merge(com.matrix1, com.matrix2, all = T)
com.matrix <- merge(com.matrix, com.matrix3, all = T)
dates <- unique(com.matrix$Date)[10:18]
com.matrix <- com.matrix[com.matrix$Date %in% dates,]

com.matrix[is.na(com.matrix)] <- 0
com.ids <- com.matrix[,1:8]

com.matrix <- com.matrix[order(com.matrix$X),]
com.matrix <- com.matrix[order(com.ids$X),]
com.matrix <- com.matrix[,-(1:8)]
com.matrix[is.na(com.matrix$X),]

com.matrix <- com.matrix %>% select(-one_of("Hole", "Note", "Oak", "Wood"))

spatial.columns = c("SubplotX", "SubplotY")

# Jost species accumulation function arguments
sites <- c("MCL", "HREC", "SFREC")
blocks <- c(1:4)
plots <- c(1:2)
spatcols <-  c("SubplotX", "SubplotY")
accumulation.func <- c("spatial", "random")
n.perm <- 9

# Create a series of figures based on different q values
for(q.value in c(1:3)){
  
  # Initializing objects for storage
  storage <- list()
  counter <- 1
  
  # Running Jost function for all unique combinations of parameters specified above
  for(site_name in sites){
    for(block in blocks){
      for(plot in plots){
        for(accum.func in accumulation.func){
          storage <- c(storage, NA)
          
          storage[[counter]] <- list(data.frame(site = site_name,
                                                block = block,
                                                plot = plot,
                                                accum = accum.func),
                                     jost_specaccum(
                                       com.data = na.omit(com.matrix[
                                         com.ids$Site == site_name & 
                                           com.ids$BlockNo == block &
                                           com.ids$PlotNo == plot,]),
                                       com.ids = com.ids[
                                         com.ids$Site == site_name & 
                                           com.ids$BlockNo == block &
                                           com.ids$PlotNo == plot,],
                                       n.perm = n.perm,
                                       q.value = q.value,
                                       spatial.columns = spatial.columns,
                                       accumulation.order = accum.func))
          
          counter <- counter + 1 
       }
      }
    }
  }
  
  # Initializing storage list
  longdata <- list(NULL)
  
  # Converting Jost output to long format
  for( i in 1:length(storage)){
    runs <- storage[[i]][[2]]$spec.accum
    dist.means <- storage[[i]][[2]]$dist.means
    id <- storage[[i]][[1]]
    
    longdata[[1]] <- rbind(longdata[[1]],
                           merge(gather(cbind(data.frame(id, run = c(1:nrow(runs)), runs)), 
                                        key = "samples", 
                                        value = "div",
                                        columns = -(1:4)),
                                 
                                 gather(cbind(data.frame(id, run = c(1:nrow(dist.means)), dist.means)), 
                                        key = "samples", 
                                        value = "dist.mean",
                                        columns = -(1:4)))
    )
    
    
  }
  
  # Converting to dataframe and cleaning values
  longdata <- longdata[[1]]
  longdata$samples <- as.numeric(gsub("X", "", longdata$samples))
  
  plotdata <- longdata %>% group_by(site, scale, accum, samples) %>% summarise(meandiv = mean(div)) 
  
  ggplot(aes(x = as.numeric((samples * (scale ^ 2))),
             y = as.numeric(meandiv),
             color = as.factor(accum)),
         data = plotdata) + 
    
    geom_line(lwd = 1.5) + 
    
    # Adding black border to regression line (makes next line clearer)
    #stat_smooth(method = "nls",
    #            formula = y ~ a*x^b,
    #            method.args = list(start = c(a = 1,
    #                                         b = 1)),
    #            se = FALSE,
    #            lwd = 1.5,
    #            aes(color = NULL,
    #                group = accum),
    #            color = "black") +
  
  # # Plotting NLS regression line
  #stat_smooth(method = "nls",
  #            formula = y ~ a*x^b,
  #            method.args = list(start = c(a = 1,
  #                                         b = 1)),
  #            se = FALSE,
  #            lwd = 1) +
  
  #stat_smooth() + 
  
  # Faceting by site and scale
  facet_grid(site~scale, scales = "free") +
    
    # Adding axis labels and title
    labs(title = "Diversity Accumulation Across Sampling Scales",
         x = "Total Area Sampled (m2)",
         y = paste("Jost Diversity at Order", q.value))
  
  ggsave(paste("./Figures/2017rawdiff_order", q.value, ".pdf", sep = ""),
         height = 10,
         width = 20)
  
  longdata %>% group_by(scale, site, accum) %>% filter(samples == min(samples)) %>% summarise(avg = mean(div))
  longdata %>% group_by(scale, site, accum) %>% filter(samples == max(samples)) %>% summarise(avg = mean(div))
  
  randdat <- longdata %>% filter(accum == "random") %>% group_by(scale, site, samples)%>%
    summarise(n.samp.rand = n(),
              mean.rand = mean(div))
  
  spatdat <- longdata %>% filter(accum == "spatial") %>% group_by(scale, site, samples) %>%
    summarise(n.samp.spat = n(),
              mean.spat = mean(div),
              dist.spat = mean(dist.mean))
  
  combdat <- merge(spatdat, randdat)
  
  combdat$diff = combdat$mean.rand - combdat$mean.spat
  
  ggplot(aes(x = samples,
             y = diff /  mean.rand,
             color = site),
         data = combdat) +
    geom_line() + 
    geom_hline(yintercept = 0) +
    ylim(-.1,.5) + 
    facet_grid(site ~ scale, scales = "free_x") + 
    # Adding axis labels and title
    labs(title = "Proportional Difference Between Random and Spatial Accumulation Curves",
         x = "Total Number of Samples",
         y = paste("Proportional Difference Jost Diversity at Order", q.value))
  
  ggsave(paste("/Figures/2017propdiff_order", q.value, ".pdf", sep = ""),
         height = 10,
         width = 20)
  
}




