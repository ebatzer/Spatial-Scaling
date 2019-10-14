setwd("~/Research Projects/Spatial Scaling")

mcl <- read.csv("MCL.csv", stringsAsFactors = F)

head(mcl)
mcl.1m <- mcl[mcl$SubplotRes == 1,]

head(mcl.1m)

mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 1] <- ((0+1)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 2] <- ((1+5)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 3] <- ((5+25)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 4] <- ((25+50)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 5] <- ((50+75)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 6] <- ((75+95)/2)
mcl.1m[,8:(ncol(mcl.1m)-1)][mcl.1m[,8:(ncol(mcl.1m)-1)] == 7] <- ((95+100)/2)

totalarea <- nrow(mcl.1m)
plots <- colSums(mcl.1m[,8:(ncol(mcl.1m) -1)] != 0)

par(mfrow=c(1,1))
abund <- colSums(mcl.1m[,8:(ncol(mcl.1m)-1)])/totalarea
barplot(abund[order(abund, decreasing=T)], las=3, ylab="Percent Cover")
plot(abund ~ plots, ylab="Percent Cover")

val <- 25

for(val in 8:51){

  x <- mcl.1m[,val]
  y <- rpois(64, mean(x))
  y <- cut(y, breaks=c(-1,0,1,5,25,50,75,95,100))
  x <- cut(x, breaks=c(-1,0,1,5,25,50,75,95,100))
  
  if( length(unique(y)) > 1){
    cat(val)
    print(chisq.test(x,y))
  }
}
mcl.1m[13]
