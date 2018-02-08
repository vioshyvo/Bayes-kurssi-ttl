# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Priorijakaumat

# aseta tyohakemisto taman tiedoston sisaltavaan kansioon (toimii Rstudiossa)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Lue kolikonheitot data frameen
df <- read.csv('finnish2e.csv', header = FALSE)
obs <- df$V1
y <- sum(obs) # laske kruunien maara
n <- length(obs)

# Priorijakaumien parametrit
alpha <- beta <- 1 
alpha_1 <- beta_1 <- 5

n <- 5 # tarkastellaan 5 ensimmaista heittoa
y <- sum(obs[1:n])
theta <- seq(0, 1, by=.001)
par(mfrow = c(1,1))
plot(theta, dbeta(theta, alpha + y, beta + n - y), type = 'l', col = 'darkblue',
     lwd = 2, ylab = 'density')
lines(theta, dbeta(theta, alpha_1 + y , beta_1 + n - y), type = 'l', col = 'violet',
      lwd = 2, ylab = 'density', main = paste('n = ', n))
legend('topleft', legend = c(paste0('Beta(', alpha,',', beta, ')'), paste0('Beta(', alpha_1,',', beta_1, ')')), lwd = 2, 
       col = c('darkblue', 'violet'),  inset = .05, bty = 'n', title = 'Priori')


par(mfrow = c(3,2))
n_all <- c(5,10,25,50,100,250)
for(n in n_all) {
  y <- sum(obs[1:n])
  alpha_n <- alpha + y
  beta_n <- beta + n - y
  plot(theta, dbeta(theta, alpha_n, beta_n), type = 'l', col = 'darkblue', ylim = c(0,13),
       lwd = 2, ylab = 'density', main = paste('n = ', n))
  lines(theta, dbeta(theta, alpha_1 + y , beta_1 + n - y), type = 'l', col = 'violet',
       lwd = 2, ylab = 'density', main = paste('n = ', n))
  abline(v = .5, lty = 2, col = 'red')
  legend('topright', legend = c(paste0('Beta(', alpha,',', beta, ')'), paste0('Beta(', alpha_1,',', beta_1, ')')), lwd = 2, 
         col = c('darkblue', 'violet'),  inset = .05, bty = 'n', title = 'Priori')
  cat('n: ', n,'\n')
  cat('Posterior mean: ', round(alpha_n / (alpha_n + beta_n), 3),'\n')
  cat('P(theta > 0.5): ', round(pbeta(0.5, alpha_n, beta_n),3), '\n\n')
  Sys.sleep(2)
}