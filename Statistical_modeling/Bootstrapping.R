#install.packages("UsingR")
library(UsingR)

x <- father.son$sheight
n <- length(x)
B <- 10000

resamples <- matrix(sample(x, n*B, replace =TRUE), B, n)
dim(resamples)

resampledMedians <- apply(resamples, 1, median)

hist(resampledMedians, prob = TRUE)
lines(density(resampledMedians), lwd =2)
