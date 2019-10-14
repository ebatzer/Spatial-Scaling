###Change in Aggregation###

#Generating plot basis
n <- 20 # number of data points
a <- 10
b <- 2
c.norm <- 25

# generate data and calculate "y"
set.seed(1)
wavelength <-1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y1 <- a*sin(b*t)+c.norm*amp # 

wavelength <- 1
amp <- 2
a <- 10
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y2 <- a*sin(b*t)+c.norm*amp # 

# generate data and calculate "x"
set.seed(1)
wavelength <- 1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x1 <- a*sin(b*t)+c.norm*amp

wavelength <- 1
amp <- 2
a <- 10
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x2 <- a*sin(b*t)+c.norm*amp

# Generate dataframe of both values
dat1 <- data.frame(x=sort(rep(1:n, n)), 
                   y=rep(1:n, n), 
                   val=rep(x1, n)[order(rep(1:n, n))] 
                   + rep(y1, n))
dat2 <- data.frame(x=sort(rep(1:n, n)), 
                   y=rep(1:n, n), 
                   val=rep(x2, n)[order(rep(1:n, n))] 
                   + rep(y2, n))
bigdat1 <- rbind(dat1,dat2)
bigdat1$set <- c(rep(1, nrow(dat1)), c(rep(2, nrow(dat2))))

library(ggplot2)
library(viridis)
p1 <- ggplot(aes(x=x,y=y,fill=val), data=dat)
p1 + geom_tile() + scale_fill_viridis() + facet_wrap(~set)

####Change in the number of individuals per plot####

#Generating plot basis
n <- 20 # number of data points

# generate data and calculate "y"
set.seed(1)
wavelength <-1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y1 <- a*sin(b*t)+c.norm*amp # 

set.seed(1)
wavelength <- 1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x1 <- a*sin(b*t)+c.norm*amp


dat3 <- data.frame(x=sort(rep(1:n, n)), 
                   y=rep(1:n, n), 
                   val=rep(x1, n)[order(rep(1:n, n))] 
                   + rep(y1, n))

n <- 10 # number of data points

wavelength <- 1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y2 <- a*sin(b*t)+c.norm*amp # 

wavelength <- 1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x2 <- a*sin(b*t)+c.norm*amp

# Generate dataframe of both values

dat4 <- data.frame(x=sort(rep(1:n, n)) * 2, 
                   y=rep(1:n, n) * 2 - .5, 
                   val=rep(x2, n)[order(rep(1:n, n))] 
                   + rep(y2, n))
bigdat2 <- rbind(dat3,dat4)
bigdat2$set <- c(rep(1, nrow(dat3)), c(rep(2, nrow(dat4))))

library(ggplot2)
library(viridis)
p1 <- ggplot(aes(x=x,y=y,fill=val), data=bigdat2)
p1 + geom_tile(width=bigdat2$set, height=bigdat2$set) + scale_fill_viridis() + facet_wrap(~set)

###Change in species area curve

#Generating plot basis
n <- 20 # number of data points
a <- 10
b <- 2

# generate data and calculate "y"
c.norm <- 25
set.seed(1)
wavelength <-1
amp <- 2
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y1 <- a*sin(b*t)+c.norm*amp # 

c.norm <- 25
wavelength <- 1
amp <- 2
a <- 6
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
y2 <- a*sin(b*t)+c.norm*amp + 1.5# 

# generate data and calculate "x"

set.seed(1)
wavelength <- 1
amp <- 2
c.norm <- 25
a <- 8
b <- 2
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x1 <- a*sin(b*t)+c.norm*amp

wavelength <- 1
amp <- 2
a <- 6
b <- 2
c.norm <- 25
t <- seq(from=0,to=wavelength*pi - ((wavelength*pi) / n),((wavelength*pi) / n))
x2 <- a*sin(b*t)+c.norm*amp + 1.5

# Generate dataframe of both values
dat5 <- data.frame(x=sort(rep(1:n, n)), 
                   y=rep(1:n, n), 
                   val=rep(x1, n)[order(rep(1:n, n))] 
                   + rep(y1, n))
dat6 <- data.frame(x=sort(rep(1:n, n)), 
                   y=rep(1:n, n), 
                   val=rep(x2, n)[order(rep(1:n, n))] 
                   + rep(y2, n))
bigdat3 <- rbind(dat5,dat6)
bigdat3$set <- c(rep(1, nrow(dat5)), c(rep(2, nrow(dat6))))

library(ggplot2)
library(viridis)
p1 <- ggplot(aes(x=x,y=y,fill=val), data=bigdat3)
p1 + geom_tile() + scale_fill_viridis() + facet_wrap(~set)

#Combining data together:

bigdat2$set <- bigdat2$set + 2
bigdat3$set <- bigdat3$set + 4

biggestdat <- rbind(bigdat1, subset(bigdat2, set == 4), subset(bigdat3, set==6))

biggestdat$size <- rep(1, nrow(biggestdat))
biggestdat$size[biggestdat$set == 4] <- 2
biggestdat$setval <- rep("NA", nrow(biggestdat))

biggestdat$setval[biggestdat$set == 1] <- "Control"
biggestdat$setval[biggestdat$set == 2] <- "Aggregation"
biggestdat$setval[biggestdat$set == 4] <- "Individuals"
biggestdat$setval[biggestdat$set == 6] <- "Abundance"

p1 <- ggplot(aes(x=x,y=y,fill=val), data=biggestdat)
p1 + geom_tile(width=biggestdat$size, height=biggestdat$size) + scale_fill_viridis(option = "magma") + facet_wrap(~set) + ggtitle("Community variation") + stat_contour(aes(x=x, y=y, z=val), data=biggestdat, bins=10, color="black")
ggsave("ComVariation.jpeg", height = 8, width=8)

#Wavelength plot
sinewaves <- subset(biggestdat, x<=1)
sinewaves <- rbind(sinewaves, subset(biggestdat, x == 2 & set==4))
sinewaves$val <- scale(sinewaves$val)

p1 <- ggplot(aes(x=y, y=val, color=factor(setval)), data=sinewaves)
p1 + geom_line(size=1.5) + geom_point(color="black") + 
  ggtitle("Wavelength of Community Variation") + 
  xlab("Distance")
ggsave("ComWavelength.jpeg", height = 8, width=8)
