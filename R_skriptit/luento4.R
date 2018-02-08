# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Luottamusvalit

# Lue kolikonheitot data frameen
df <- read.csv('finnish2e.csv', header = FALSE)
obs <- df$V1
y <- sum(obs) # laske kruunien maara
n <- length(obs)

# Priorijakaumien parametrit
alpha <- beta <- 1 
alpha_1 <- beta_1 <- 5

# Aseta uskottavuustaso
alpha_conf <- .5
alpha_conf / 2
1 - alpha_conf / 2

# Paivita posteriorijakauman parametrit
n <- 5 # tarkastellaan 5 ensimmaista heittoa
y <- sum(obs[1:n])
alpha_n <- alpha + y
beta_n <- beta + n - y

#########################################################################
# Symmetrinen uskottavuusvali 

theta <- seq(0, 1, by=.001)
par(mfrow = c(1,1), mar = c(5,4,4,2))
plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
     lwd = 2, ylab = 'density', bty = 'n', main = paste0(100 * (1-alpha_conf), '% equal-tailed CI'), bty = 'n')

lower <- qbeta(alpha_conf / 2, alpha_n, beta_n)
upper <- qbeta(1 - alpha_conf / 2, alpha_n, beta_n)
y_val <- dbeta(theta, alpha_n, beta_n)
polygon(c(lower, theta[theta >= lower & theta <= upper], upper), c(0, y_val[theta >= lower & theta <= upper], 0),
        col = 'skyblue', lwd = 2,  border = 'darkblue')
abline(v = 1, lty = 2)

#########################################################################
# HPD uskottavuusvali 

# install.packages('HDInterval')
n_sim <- 1e6
theta_sim <- rbeta(n_sim, alpha_n, beta_n)
dens <- density(theta_sim)
HPD_region <- HDInterval::hdi(dens, allowSplit = TRUE, credMass = 1 - alpha_conf )
height <- attr(HPD_region, 'height')

HPD_region <- HDInterval::hdi(qbeta, 1 - alpha_conf, shape1 = alpha_n, shape2 = beta_n)
lower_HPD <- HPD_region[1]
upper_HPD <- HPD_region[2]

x_coord <- c(lower_HPD, theta[theta >= lower_HPD & theta <= upper_HPD], upper_HPD)
y_coord <- c(0, y_val[theta >= lower_HPD & theta <= upper_HPD], 0)

# HPD interval
par(mfrow = c(2,1), mar = c(5,4,4,2))
plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
     lwd = 2, ylab = 'density', bty = 'n', main = paste0(100 * (1-alpha_conf), '% HPD interval'))
polygon(x_coord, y_coord, col = 'skyblue', lwd = 2,  border = 'darkblue')
abline(h = height, col = 'blue', lty = 2, lwd = 2)
abline(v = 1, lty = 2)

# Equal-tailed CI
plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
     lwd = 2, ylab = 'density', bty = 'n', main = paste0(100 * (1-alpha_conf), '% equal-tailed CI'))
lower <- qbeta(alpha_conf / 2, alpha_n, beta_n)
upper <- qbeta(1 - alpha_conf / 2, alpha_n, beta_n)
y_val <- dbeta(theta, alpha_n, beta_n)
polygon(c(lower, theta[theta >= lower & theta <= upper], upper), c(0, y_val[theta >= lower & theta <= upper], 0),
        col = 'skyblue', lwd = 2,  border = 'darkblue')
abline(v = 1, lty = 2)

cat(100 * (1-alpha_conf), '% HPD interval: (', round(lower_HPD,3), ',', round(upper_HPD,3), ')\n', sep = '')
cat(100 * (1-alpha_conf), '% equal-tailed CI: (', round(lower,3), ',', round(upper,3), ')\n', sep = '')

#########################################################################
# (Symmetriset) uskottavuusvalit eri otoskoolla 

par(mfrow = c(3,2))
n_all <- c(5,10,25,50,100,250)
for(n in n_all) {
  y <- sum(obs[1:n])
  alpha_n <- alpha + y
  beta_n <- beta + n - y
  plot(theta, dbeta(theta, alpha_n, beta_n), type = 'l', col = 'darkblue', ylim = c(0,13),
       lwd = 2, ylab = 'density', main = paste0(100 * (1-alpha_conf), '% equal-tailed CI, n = ', n))
  
  lower <- qbeta(alpha_conf / 2, alpha_n, beta_n)
  upper <- qbeta(1 - alpha_conf / 2, alpha_n, beta_n)
  y_val <- dbeta(theta, alpha_n, beta_n)
  polygon(c(lower, theta[theta >= lower & theta <= upper], upper), c(0, y_val[theta >= lower & theta <= upper], 0),
          col = 'skyblue', lwd = 2,  border = 'darkblue')
  abline(v = .5, lty = 2, col = 'red')
  
  cat('n: ', n,'\n')
  cat(1-alpha_conf, ' equal-tailed CI: (', round(lower, 3), ',', round(upper, 3), ')\n\n', sep = '')
  Sys.sleep(2)
}