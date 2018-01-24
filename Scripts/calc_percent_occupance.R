# Calling libraries
library(dplyr)
library(ggplot2)

# Reading in files
com.matrix <- read.csv("./2016/alldatacombined2016.csv", stringsAsFactors = F)
com.ids <- read.csv("./2016/alldataidentification2016.csv", stringsAsFactors = F)

# Grouping by site, calculating total number of samples
com.binary <- data.frame(matrix(if_else(com.matrix[,-1] > 0, 1, 0), nrow = nrow(com.matrix), byrow = F))
colnames(com.binary) <- colnames(com.matrix)[-1]

# Counting the number of cells occupied per species
cells_occupied <- cbind(com.ids, com.binary) %>% group_by(site_code, subplotres) %>% 
  summarise_at(vars(colnames(com.binary)),
               funs(occ = "sum"))

# Counting the total numbers of cells per subplot resolution
total_cells <- com.ids %>% group_by(site_code, subplotres) %>% 
  summarise(cells = n())

# Merging and generating proportional occupancy
occupancy <- merge(cells_occupied, total_cells, all = TRUE) %>% group_by(site_code, subplotres) %>%
  transmute_at(.vars = colnames(cells_occupied)[-c(1:3)],
               funs(prop = . / cells))

# Converting to long format
library(tidyr)
long.occupancy <- gather(occupancy,
                         key = "species",
                         value = "occ",
                         -c(1:3))

long.occupancy$species <- gsub("_occ_prop", "", long.occupancy$species)
com.matrix$BriMin <- as.numeric(com.matrix$BriMin)

covsum <- data.frame(cbind(com.ids, com.matrix)) %>% 
  gather(key = "species", value = "cover", -c(1:21)) %>% 
  filter(subplotres == 1) %>%
  group_by(site_code, species) %>%
  summarise(meancov = mean(cover),
            totcov = sum(cover)) %>%
  mutate(propcov = totcov / sum(totcov))

covocc <- merge(covsum, long.occupancy)

ggplot(aes(x = propcov * 100,
       y = occ,
       color = species),
       data = covocc) + 
  geom_point() + 
  guides(color = "none") + 
  facet_grid(site_code~subplotres)

ggplot(aes(x = propcov * 100,
           y = occ,
           color = species),
       data = covocc) + 
  geom_point() + 
  guides(color = "none") +
  facet_wrap(~ subplotres)
