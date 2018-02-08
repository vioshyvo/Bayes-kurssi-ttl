# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Tunnuslukuja & todennakoisyyksia posteriorijakaumasta

# Lue kolikonheitot data frameen
df <- read.csv('finnish2e.csv', header = FALSE)
obs <- df$V1
y <- sum(obs) # laske kruunien maara
n <- length(obs)

# Priorijakaumien parametrit
alpha <- beta <- 1 
alpha_1 <- beta_1 <- 5

# Aseta luottamustaso
alpha_conf <- .5
alpha_conf / 2
1 - alpha_conf / 2

# Paivita posteriorijakauman parametrit
n <- 5 # tarkastellaan 5 ensimmaista heittoa
y <- sum(obs[1:n])
alpha_n <- alpha + y
beta_n <- beta + n - y

#########################################################################
# Tunnuslukuja posteriorijakaumasta

beta_mean <- function(alpha, beta) alpha / (alpha + beta)
beta_mode <- function(alpha, beta) (alpha - 1) / (alpha + beta - 2)
beta_sd <- function(alpha, beta) sqrt( alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1)) )

(bmean <- beta_mean(alpha + y, beta + n - y)) # posteriorijakauman odotusarvo
(bmode <- beta_mode(alpha + y, beta + n - y)) # posteriorijakauman moodi
(bsd <- beta_sd(alpha + y, beta + n - y))     # posteriorijakauman keskihajonta

theta <- seq(0, 1, by=.001)
par(mfrow = c(1,1))
plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
     lwd = 2, ylab = 'density', main = paste0('n = ', n), bty = 'n')
points(bmode, dbeta(bmode, alpha + y, beta + n - y), col = 'red', pch = 4, lwd = 2)
points(bmean, dbeta(bmean, alpha + y, beta + n - y), col = 'green', pch = 4, lwd = 2)
segments(bmode, 0 , bmode, dbeta(bmode, alpha + y, beta + n - y), lty = 2, col = 'red', lwd = 2)
segments(bmean, 0 , bmean, dbeta(bmean, alpha + y, beta + n - y), lty = 2, col = 'green', lwd = 2)
legend('topleft', legend = c(paste('mean =', round(bmean,3) ) , paste('mode = ', round(bmode, 3)), paste('sd =', round(bsd, 3))),
       col = c('green', 'red', 'white'), lwd = 2, lty = 2, bty = 'n', inset = .05, cex = 1.5)

n_all <- c(5,10,25,50,100,250)
par(mfrow = c(3,2))

for(n in n_all) {
  y <- sum(obs[1:n])
  alpha_n <- alpha + y
  beta_n <- beta + n - y
  
  (bmean <- beta_mean(alpha + y, beta + n - y))
  (bmode <- beta_mode(alpha + y, beta + n - y))
  (bsd <- beta_sd(alpha + y, beta + n - y))
  
  theta <- seq(0, 1, by=.001)
  plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
       lwd = 2, ylab = 'density', main = paste0('n = ', n), bty = 'n')
  points(bmode, dbeta(bmode, alpha + y, beta + n - y), col = 'red', pch = 4, lwd = 2)
  points(bmean, dbeta(bmean, alpha + y, beta + n - y), col = 'green', pch = 4, lwd = 2)
  segments(bmode, 0 , bmode, dbeta(bmode, alpha + y, beta + n - y), lty = 2, col = 'red', lwd = 2)
  segments(bmean, 0 , bmean, dbeta(bmean, alpha + y, beta + n - y), lty = 2, col = 'green', lwd = 2)
  legend('topleft', legend = c(paste('mean =', round(bmean,3) ) , paste('mode = ', round(bmode, 3)), paste('sd =', round(bsd, 3))),
         col = c('green', 'red', 'white'), lwd = 2, lty = 2, bty = 'n', inset = 0, cex = 1.5)
}


#########################################################################
# Todennakoisyyksia posteriorijakaumasta

# P(theta > 0.5)
1 - pbeta(.5, alpha_n, beta_n)

# P(0.4 < theta < 0.6)
pbeta(.6, alpha_n, beta_n) - pbeta(.4, alpha_n, beta_n)

lower <- .5
upper <- 1

n_all <- c(5,10,25,50,100,250)
par(mfrow = c(3,2))

for(n in n_all) {
  y <- sum(obs[1:n])
  alpha_n <- alpha + y
  beta_n <- beta + n - y
  
  plot(theta, dbeta(theta, alpha_n, beta_n), type = 'l', col = 'darkblue', ylim = c(0,13),
       lwd = 2, ylab = 'density', main = paste0('n = ', n))
  
  y_val <- dbeta(theta, alpha_n, beta_n)
  polygon(c(lower, theta[theta >= lower & theta <= upper], upper), c(0, y_val[theta >= lower & theta <= upper], 0),
          col = 'skyblue', lwd = 2,  border = 'darkblue')
  abline(v = c(lower,upper), lty = 2, col = 'red')
  
  cat('n: ', n,'\n')
  ci_length <- pbeta(upper, alpha_n, beta_n) - pbeta(lower, alpha_n, beta_n) 
  cat('P(0.4 < theta < 0.6): ', round(ci_length, 3), '\n\n')
  if(upper == 1) {
    label <- bquote(paste('P(', theta, '>', .(lower),')=', .(round(ci_length, 3))))
  } else if(lower == 1) {
    label <- bquote(paste('P(', theta, '<', .(upper),')=', .(round(ci_length, 3))))
  } else {
    label <- bquote(paste('P(', .(lower), '<', theta, '<', .(upper),')=', .(round(ci_length, 3))))  
  }
  text(x = 0.25, y = 12, label, cex = 1.5)
}



