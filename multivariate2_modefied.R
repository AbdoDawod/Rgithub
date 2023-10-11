#####*The code is designed for simulating Hotlling T2 multivariate control chart under normality conditions
#####* writer: Abd-uljbbar Dawod
#####* 07-09-2023
rm(list=ls(all=TRUE))
setwd('C:\\Users\\admin\\Documents\\MEGA\\Unideb\\Rgit\\Hotllingt2')
set.seed(07092023)
library(MASS)

p <-  2
m <- 5
n <- 1e4
alpha <- 0.005 #
mu0 <- rep(0,p)  # Means of the variables
shift <-  0 #c(0,0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 5)
off_diag_value <- 0
  
# Create the covariance matrix
sigma0 <- diag(p)
sigma0[lower.tri(sigma0)] <- off_diag_value
sigma0[upper.tri(sigma0)] <- off_diag_value

UCL <-p*(m-1)*(n-1)/(m*n-n-p+1)*qf(1- alpha^p, p, (m*n-n-p+1))

ARL <- numeric(length(shift))
for( k in seq_along(shift) )
{
  mu <- mu0 + shift[k]
arlen   <-  numeric(n)
for (i in seq(n))
{
  rl  <-  1; cnt  <-  0
  while (cnt < 1)
  {
    X <-mvrnorm(n = m, mu = mu, Sigma = sigma0)
    T2 <- m*mahalanobis(colMeans(X), center = mu0, cov = sigma0 )
    ifelse(T2 > UCL,{ cnt = 1
                      break}, {rl = rl + 1})
  }
  arlen[i] <- rl
}
ARL[k]<- mean(arlen)
}
Result <- cbind(round(shift,2), round(ARL,2))
#write.table(Result , 'Results.txt', sep="\t", row.names=F, col.names=FALSE, append = TRUE)
#write.table('\n\n' , 'Results.txt', sep=" ", row.names=F, col.names=FALSE, append = TRUE)
Result 