dim(T1);dim(T2)
#View(T1);View(T2)
x <- dim(T1)[1]
y <- dim(T1)[2]
T12 <- cbind(T1,T2);dim(T12)
#View(T12)
library(psych)
t= 2
icc <- c(1:y)
for(i in 1:y) {icc[i] <- ICC(T12[,c(i,i+y)])$results$ICC[t]}
#icc
mean(icc);median(icc)
l <- length(which(icc >= 0.75)) ;l
s <- length(which(icc <= 0.75)); s
