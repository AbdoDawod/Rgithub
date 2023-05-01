library(astsa)

xt = log(gnp)
xt2 <- diff(xt)

par(mfrow=c(2,1),mar=c(2,1,2,1))
plot(xt)
plot(xt2)