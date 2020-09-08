# Finding Last Year's Species

setwd("../2018")

peak <- read.csv("merged_data_2018_peak.csv")

library(tidyverse)

ly_specs <- peak %>% 
  select(-PlotNo, - X, -Subplot.No, - Subplot.X, - Subplot.Y) %>%
  group_by(SiteName, BlockNo) %>%
  summarise_all(sum) %>%
  gather(key = "species", value =  "cover", - SiteName, -BlockNo) %>%
  arrange(SiteName, BlockNo, desc(cover)) %>%
  filter(cover != 0)

write.csv("../2019/ly_specs.csv", x = ly_specs)
