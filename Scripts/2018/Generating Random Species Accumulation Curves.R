
# Random accumulation curves
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
source("scripts/ens_specaccum.R")

final_merged <- read.csv("./final_2018.csv", header = TRUE, stringsAsFactors = FALSE)
head(final_merged)[,1:20]

com.mat <- final_merged %>% select(- one_of("site", "block", "plot", "subplot", "x", "y", 
                                            "note", "index", "year", "sitename", "dateestablished", 
                                            "plotlocation", "fertilized", "X"))
com.attr <-  final_merged %>% select(one_of("site", "block", "plot", "subplot", "x", "y", 
                                            "note", "index", "year", "sitename", "dateestablished", 
                                            "plotlocation", "fertilized"))

# Accumulation at q = 0
specaccum_random <- function(qval, n.perm){
  
  output <- list(NA)
  counter <- 1
  
  for(SiteName in unique(com.attr$site)){
    for(YearVal in unique(com.attr$year)){
      for(BlockVal in c(1:4)){
        cat(paste("\n", SiteName, YearVal, BlockVal, "\n"))
        permoutput <- list()
        pb <- txtProgressBar(style = 3)
        
        for(permno in 1:n.perm){
          
          # Fertilized random aggregation
          F_agg <- jost_specaccum_v2(
            com.data = com.mat[com.attr$site == SiteName &
                                 com.attr$block == BlockVal &
                                 com.attr$year == YearVal &
                                 com.attr$fertilized == 1,][sample(c(1:64), 64),],
            com.ids = com.attr[com.attr$site == SiteName &
                                 com.attr$block == BlockVal &
                                 com.attr$year == YearVal &
                                 com.attr$fertilized == 1,],
            q.value = qval,
            spatial.columns = c("x", "y"))
          
          # Unfertilized spatial aggregation
          UF_agg <- jost_specaccum_v2(
            com.data = com.mat[com.attr$site == SiteName &
                                 com.attr$block == BlockVal &
                                 com.attr$year == YearVal &
                                 com.attr$fertilized == 0,][sample(c(1:64), 64),],
            com.ids = com.attr[com.attr$site == SiteName &
                                 com.attr$block == BlockVal &
                                 com.attr$year == YearVal &
                                 com.attr$fertilized == 0,],
            q.value = qval,
            spatial.columns = c("x", "y"))
          
          out <- cbind(data.frame(site = rep(as.character(SiteName), 2),
                                  block = rep(as.character(BlockVal), 2),
                                  year = rep(as.character(YearVal), 2),
                                  fert = c(1,0),
                                  stat = c("mean", "mean")),
                       perm = c(permno, permno),
                       rbind(F_agg[[1]], UF_agg[[1]]))
          
          permoutput[[permno]] <- data.frame(out)
          
          setTxtProgressBar(pb, permno / n.perm)
        }
        
        output[[counter]] = data.frame(bind_rows(permoutput))
        counter <- counter + 1
        
      }
    }
  }
  
  return(output)
}

# q0_raw <- reduce(specaccum_random(qval = 0, n.perm = 999), bind_rows)
# colnames(q0_raw) <- gsub("X", "", colnames(q0_raw))
# 
# q0_random <- q0_raw %>% 
#   gather(key = "samp", value = "div", -(1:6)) %>%
#   filter(! ((site == "HREC") & (block == 1))) 
# 
# calc_diff <- function(x){
#   return(data.frame(div_u = x$div[x$fert == 0],
#                     div_f = x$div[x$fert == 1],
#                     diff = x$div[x$fert == 0] - x$div[x$fert == 1],
#                     diff_frac = (x$div[x$fert == 0] - x$div[x$fert == 1]) / x$div[x$fert == 0]))
# }
# 
# 
# q0_random_summary <- q0_random %>% group_by(site, block, year, samp, perm) %>%
#   do(calc_diff(.))
# 
# q0_random_summary %>% filter(samp == 1 | samp == 64)
# 
# q0_random_summary %>%
#   ggplot(aes(x = as.numeric(samp))) +
#   geom_line(aes(y = -diff_frac, linetype = paste(site, block), color = year))

# write.csv(x = q0_raw, "randompermutations_q0.csv")
# 
# remove(q0_raw)

# Accumulation at q = 1
q1_raw <- reduce(specaccum_random(qval = 1, n.perm = 999), bind_rows)
colnames(q1_raw) <- gsub("X", "", colnames(q1_raw))

# q1_random <- q1_raw %>% 
#   gather(key = "samp", value = "div", -(1:6)) %>%
#   filter(! ((site == "HREC") & (block == 1))) 
# 
# calc_diff <- function(x){
#   return(data.frame(div_u = x$div[x$fert == 0],
#                     div_f = x$div[x$fert == 1],
#                     diff = x$div[x$fert == 0] - x$div[x$fert == 1],
#                     diff_frac = (x$div[x$fert == 0] - x$div[x$fert == 1]) / x$div[x$fert == 0]))
# }
# 
# 
# q1_random_summary <- q1_random %>% group_by(site, block, year, samp, perm) %>%
#   do(calc_diff(.))
# 
# q1_random_summary %>% filter(samp == 1 | samp == 64)
# 
# q1_random_summary %>%
#   ggplot(aes(x = as.numeric(samp))) +
#   geom_line(aes(y = -diff_frac, linetype = paste(site, block), color = year))

write.csv(x = q1_raw, "randompermutations_q1.csv")

remove(q1_raw)

# Accumulation at q = 2
q2_raw <- reduce(specaccum_random(qval = 2, n.perm = 999), bind_rows)
colnames(q2_raw) <- gsub("X", "", colnames(q2_raw))


# 
# q2_random <- q2_raw %>% 
#   gather(key = "samp", value = "div", -(1:6)) %>%
#   filter(! ((site == "HREC") & (block == 1))) 
# 
# calc_diff <- function(x){
#   return(data.frame(div_u = x$div[x$fert == 0],
#                     div_f = x$div[x$fert == 1],
#                     diff = x$div[x$fert == 0] - x$div[x$fert == 1],
#                     diff_frac = (x$div[x$fert == 0] - x$div[x$fert == 1]) / x$div[x$fert == 0]))
# }
# 
# q2_random_summary <- q2_random %>% group_by(site, block, year, samp, perm) %>%
#   do(calc_diff(.))
# 
# q2_random_summary %>% filter(samp == 1 | samp == 64)
# 
# q2_random_summary %>%
#   ggplot(aes(x = as.numeric(samp))) +
#   geom_line(aes(y = -diff_frac, linetype = paste(site, block), color = year))

write.csv(x = q2_raw, "randompermutations_q2.csv")

remove(q2_raw)




