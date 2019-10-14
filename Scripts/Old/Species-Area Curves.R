# Set working directory:
setwd("~/Research Projects/Spatial Scaling")

mcl <- read.csv("MCL_Richness_Data.csv", header= T, stringsAsFactors = F)
sfrec <- read.csv("SFREC_Richness_Data.csv", header= T, stringsAsFactors = F)
hrec <- read.csv("HREC_Richness_Data.csv", header= T, stringsAsFactors = F)

colnames(mcl)
richdat <-rbind(mcl[,c(2:8,c(ncol(mcl), ncol(mcl) - 1))], 
      sfrec[,c(2:8,c(ncol(sfrec), ncol(sfrec) - 1))])
richdat <- rbind(richdat,
                 hrec[,c(2:8,c(ncol(hrec), ncol(hrec) - 1))])

library(ggplot2)
library(viridis)

p1 <- ggplot(aes(x=SubplotArea, 
                 y=Richness, 
                 color=Site), 
             data=subset(richdat, SubplotArea < 65))
p1 + 
  theme_bw()+
  geom_point(color="black", size=2) +
  geom_point()+
  facet_wrap(~Site) +
  scale_color_viridis(discrete = T) + 
  geom_smooth(method="nls", 
              formula=y ~ z * x ^ c, # this is an nls argument
              method.args = list(start=c(z=1,c=1)), # this too
              se=FALSE,
              color="black") +
  xlab("Subplot Area (m^2)") +
  ylab("Species Richness") +
  theme(legend.position='none')

ggsave("Richness Curves.jpeg", height = 4, width=12)


p2 <- ggplot(aes(x=log10(SubplotArea), 
                 y=log10(Richness), 
                 color=Site), 
             data=subset(richdat, SubplotArea < 65))
p2 + 
  theme_bw()+
  geom_point(color="black", size=2) +
  geom_point()+
  facet_wrap(~Site) +
  scale_color_viridis(discrete = T) + 
  geom_smooth(method="lm") +
  xlab("Log2 Subplot Area (m^2)") +
  ylab("Species Richness") +
  theme(legend.position='none')

ggsave("Log-Scale Richness Curves.jpeg", height = 4, width=12)

