################################################################################
# Calculating Differences Among Datasets #######################################
################################################################################

# Calling libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Reading in files
com.matrix <- read.csv("./2016/alldatacombined2016.csv", stringsAsFactors = F)
com.ids <- read.csv("./2016/alldataidentification2016.csv", stringsAsFactors = F)

# Creating diversity comparison function:
calc_rankdiff <- function(com.data1, # Dataset 1
                          com.data2){ # Dataset 2)
  
  prop.df1 <- colSums(com.data1) / sum(colSums(com.data1))
  prop.df2 <- colSums(com.data2) / sum(colSums(com.data2))
  
  names(prop.df1) <- colnames(com.data1)
  names(prop.df2) <- colnames(com.data2)
  
  prop.df1 <- sort(prop.df1, decreasing = T)
  prop.df2 <- sort(prop.df2, decreasing = T)

  return(rbind(prop.df1, prop.df2))
}

#HREC
hrec_comp <- rbind(calc_rankdiff(com.matrix[com.ids$site_code == "HREC" & com.ids$subplotres == 1,-1],
                                 com.matrix[com.ids$site_code == "HREC" & com.ids$subplotres == .5,-1]),
                   calc_rankdiff(com.matrix[com.ids$site_code == "HREC" & com.ids$subplotres == 1,-1],
                            com.matrix[com.ids$site_code == "HREC" & com.ids$subplotres == .25,-1])[2,])

rownames(hrec_comp) <- NULL
hrec_comp <- cbind(site = rep("HREC",3), scale = c(1,.5,.25), hrec_comp)

# SFREC
sfrec_comp <- rbind(calc_rankdiff(com.matrix[com.ids$site_code == "SFREC" & com.ids$subplotres == 1,-1],
                                 com.matrix[com.ids$site_code == "SFREC" & com.ids$subplotres == .5,-1]),
                   calc_rankdiff(com.matrix[com.ids$site_code == "SFREC" & com.ids$subplotres == 1,-1],
                                 com.matrix[com.ids$site_code == "SFREC" & com.ids$subplotres == .25,-1])[2,])

rownames(sfrec_comp) <- NULL
sfrec_comp <- cbind(site = rep("SFREC",3), scale = c(1,.5,.25), sfrec_comp)

# MCL
mcl_comp <- rbind(calc_rankdiff(com.matrix[com.ids$site_code == "MCL" & com.ids$subplotres == 1,-1],
                                 com.matrix[com.ids$site_code == "MCL" & com.ids$subplotres == .5,-1]),
                   calc_rankdiff(com.matrix[com.ids$site_code == "MCL" & com.ids$subplotres == 1,-1],
                                 com.matrix[com.ids$site_code == "MCL" & com.ids$subplotres == .25,-1])[2,])

rownames(mcl_comp) <- NULL
mcl_comp <- cbind(site = rep("MCL",3), scale = c(1,.5,.25), mcl_comp)

# All together:
all_prop <- rbind(data.frame(mcl_comp), data.frame(hrec_comp), data.frame(sfrec_comp))


prop_long <- data.frame(all_prop) %>% gather(key = "spec",
                    value = "prop",
                    -c(1:2))

sumstats <- prop_long %>% group_by(site, spec) %>% filter(scale == 1) %>% summarize(maxval = max(as.numeric(prop)))
sumstats <- sumstats %>% group_by(site) %>% mutate(rankval = base::rank(maxval, ties.method = "first"))
prop_long <- merge(prop_long, sumstats)

# Plotting: 

prop_long %>% filter(prop > 0) %>%
  ggplot(aes(x = abs(max(rankval + 1) - rankval),
           y = as.numeric(prop),
           fill = scale)) + 
  geom_bar(stat = "identity", alpha = .3, position = PositionIdentity) + 
  geom_line(aes(color = scale), lwd = .5) + 
  facet_wrap(~site) +
  theme(legend.key = element_blank(),
        panel.background = element_blank()) +
  xlab("Species Rank Abundance") +
  ylab("Proportional Abundance (of Total)")

ggsave("TotalRankAbundance.pdf",
       height = 8,
       width = 16)

prop_long %>% filter(prop > 0) %>%
  ggplot(aes(x = abs(max(rankval + 1) - rankval),
             y = as.numeric(prop) - maxval,
             color = scale)) + 
  geom_bar(stat = "identity", alpha = .3, position = PositionIdentity) + 
  facet_wrap(~site) +
  theme(legend.key = element_blank(),
        panel.background = element_blank()) +
  xlab("Species Rank Abundance") +
  ylab("Absolute Difference in Proportional Abundance From Largest Scale")

ggsave("DifferenceInAbsAbundance.pdf",
       height = 8,
       width = 16)


prop_long %>% filter(prop > 0) %>%
  ggplot(aes(x = abs(max(rankval + 1) - rankval),
             y = (as.numeric(prop) / maxval) - 1,
             color = scale)) + 
  geom_bar(stat = "identity", alpha = .3, position = PositionIdentity) + 
  facet_wrap(~site) +
  theme(legend.key = element_blank(),
        panel.background = element_blank()) +
  xlab("Species Rank Abundance") +
  ylab("Proportional Difference in Proportional Abundance From Largest Scale")

ggsave("DifferenceInPropAbundance.pdf",
       height = 8,
       width = 16)


### Determining correlation

prop_long <- prop_long %>% filter(prop > 0) %>% mutate(propdiff = (as.numeric(prop) / maxval) - 1)
c(cor(prop_long$rankval[prop_long$scale == .5 & prop_long$site == "HREC"], 
    prop_long$propdiff[prop_long$scale == .5 & prop_long$site == "HREC"]),
cor(prop_long$rankval[prop_long$scale == .25 & prop_long$site == "HREC"], 
    prop_long$propdiff[prop_long$scale == .25 & prop_long$site == "HREC"]),
cor(prop_long$rankval[prop_long$scale == .5 & prop_long$site == "SFREC"], 
    prop_long$propdiff[prop_long$scale == .5 & prop_long$site == "SFREC"]),
cor(prop_long$rankval[prop_long$scale == .25 & prop_long$site == "SFREC"], 
    prop_long$propdiff[prop_long$scale == .25 & prop_long$site == "SFREC"]),
cor(prop_long$rankval[prop_long$scale == .5 & prop_long$site == "MCL"], 
    prop_long$propdiff[prop_long$scale == .5 & prop_long$site == "MCL"]),
cor(prop_long$rankval[prop_long$scale == .25 & prop_long$site == "MCL"], 
    prop_long$propdiff[prop_long$scale == .25 & prop_long$site == "MCL"]))
