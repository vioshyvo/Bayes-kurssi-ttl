# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# MCMC-demo: Gibbs-sampleri


y <- c(0,0) # simuloitavan multinormaalijakauman odotusarvo
rho <- -0.7 # simuloitavan multinormaalijakauman korrelaatiotermi

# Gibbs-samplerin paivityskaavat
mu1_update <- function(y, rho, mu2) rnorm(1, y[1] + rho * (mu2 - y[2]), sqrt(1 - rho^2))
mu2_update <- function(y, rho, mu1) rnorm(1, y[2] + rho * (mu1 - y[1]), sqrt(1 - rho^2))

######################################################################3
# Gibbs-sampler in slow motion

n_sim <- 30 # samplerin iteraatioiden maara
mu1 <- mu2 <- numeric(n_sim)

# aseta aloituspiste samplerille
mu1[1] <- 2
mu2[1] <- 2

sleep <- 1
par(mfrow = c(1,1))
plot(mu1[1], mu2[1], pch = 4, lwd = 2, xlim = c(-4,4), ylim = c(-4,4), asp = 1, 
     xlab = expression(mu[1]), ylab = expression(mu[2]), bty = 'n', col = 'darkred')

for(i in 2:n_sim) {
  mu1[i] <- mu1_update(y, rho, mu2[i-1])
  lines(c(mu1[i-1], mu1[i]), c(mu2[i-1], mu2[i-1]), type = 'l', col = 'darkred')
  Sys.sleep(sleep)
  mu2[i] <- mu2_update(y, rho, mu1[i])
  lines(c(mu1[i], mu1[i]), c(mu2[i-1], mu2[i]), type = 'l', col = 'darkred')
  points(mu1[i], mu2[i], pch = 16, col = 'darkred')
  Sys.sleep(sleep)
}

plot(mu1[10:length(mu1)], mu2[10:length(mu2)], pch = 16, col = 'darkgreen',
     xlim = c(-4,4), ylim = c(-4,4), asp = 1, xlab = expression(mu[1]), 
     ylab = expression(mu[2]), bty = 'n', cex = 0.7)

#####################################################
# Gibbs-samplerin jalki, kun n = 10, 100 tai 1000

n_sim <- 1000
mu1 <- mu2 <- numeric(n_sim2)
mu1[1] <- 2
mu2[1] <- 2

for(i in 2:n_sim) {
  mu1[i] <- mu1_update(y, rho, mu2[i-1])
  mu2[i] <- mu2_update(y, rho, mu1[i])
}

draw_gibbs <- function(mu1, mu2, idx_end, points = FALSE) {
  plot(mu1[1], mu2[1], pch = 4, lwd = 2, xlim = c(-4,4), ylim = c(-4,4), asp = 1, 
       xlab = expression(mu[1]), ylab = expression(mu[2]), bty = 'n', col = 'darkred', main = paste('n =', idx_end)) 
  for(j in 2:idx_end) {
    lines(c(mu1[j-1], mu1[j]), c(mu2[j-1], mu2[j-1]), type = 'l', col = 'darkred')
    lines(c(mu1[j], mu1[j]), c(mu2[j-1], mu2[j]), type = 'l', col = 'darkred')
    if(points) points(mu1[j], mu2[j], pch = 16, col = 'darkred')
  }
} 

draw_sample <- function(mu1, mu2, ...) {
  plot(mu1, mu2, pch = 16, col = 'darkgreen',
       xlim = c(-4,4), ylim = c(-4,4), asp = 1, xlab = expression(mu[1]), 
       ylab = expression(mu[2]), bty = 'n', cex = 0.7, ...)
}

par(mfrow = c(2,2), mar = c(2,2,4,4))
draw_gibbs(mu1, mu2, 10, points = TRUE)
draw_gibbs(mu1, mu2, 100)
draw_gibbs(mu1, mu2, n_sim)
draw_sample(mu1[10:length(mu1)], mu2[10:length(mu2)], main = paste('n =', n_sim))

#####################################################
# vertaa MCMC-otosta i.i.d.-otokseen

# install.packages('MASS')
Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
X <- MASS::mvrnorm(n_sim, y, Sigma)

par(mfrow = c(1,2), mar = c(2,2,4,4))
draw_sample(mu1[10:length(mu1)], mu2[10:length(mu2)], main = 'MCMC')
draw_sample(X[ ,1], X[ ,2], main ='i.i.d.')
