# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Yhden otoksen "t-testi" 

##############################################################
# Posteriori yhden otoksen keskiarvolle (ja keskihajonnalle)

df <- read.csv( file="TwoGroupIQ.csv" )
y <- df$Score[df$Group=="Smart Drug"]

smart_list <- list(n = length(y), y = y, y_mean = mean(y), y_sd = sd(y))

fit <- stan('smart.stan', iter = 1e4, data = smart_list)
plot(fit)

#############################################################
# Piirrä parametrien ja effect size:n posterorit 95% uskottavuusvaleineen,
# havaittu aineisto ja posterioriennustejakauma
# Verrataan keskiarvoiseen alykkyysosamaaraan 100

sim <- extract(fit)
sum <- summary(fit)$summary
ci95 <- sum[,c(4,8)]


par(mfrow = c(3,2))
hist(sim$mu, col = 'skyblue', breaks = 30, probability = TRUE, main = 'Odotusarvo')
segments(ci95["mu", 1], 0, ci95["mu", 2], 0, lwd = 6)
abline(v = 100, lty = 2, col = 'red', lwd = 2)

n_sim <- length(sim$mu)
hist(y, col = 'pink', breaks = 30, probability = TRUE, main = 'Data w/ posterior predictive', xlim = c(0,200))
for(i in 1:100) {
  y_pred <- rnorm(n_sim, sim$mu, sim$sigma)
  dens <- density(y_pred)
  lines(dens, col = 'blue')
}

hist(sim$sigma, col = 'skyblue', breaks = 30, probability = TRUE, xlim = c(12,35), 
     main = 'Keskihajonta')
segments(ci95["sigma", 1], 0, ci95["sigma", 2], 0, lwd = 6)
abline(v = 15, lty = 2, col = 'red', lwd = 2)

hist(y_pred, col = 'skyblue', breaks = 50, probability = TRUE, main = 'Posterior predictive', xlim = c(0,200), ylim = c(0, .03))

hist(sim$effect_size, col = 'skyblue', breaks = 30, probability = TRUE,
     main = 'Effect size')
segments(ci95["effect_size", 1], 0, ci95["effect_size", 2], 0, lwd = 6)
abline(v = 0, lty = 2, col = 'red', lwd = 2)


######################################################################3
# kokeillaan t:n jakaumaa normaalijakauman sijaan

# Nama luennolla vasatyt koodiet toimivat nakojaan tyokoneella aivan moitteettomasti
# Lievaa demoefektia havaittavissa...

fit <- stan('smart2.stan', data = smart_list, iter = 1e4)

sim <- extract(fit)

sum <- summary(fit)$summary
ci95 <- sum[,c(4,8)]

par(mfrow = c(3,2))
hist(sim$mu, col = 'skyblue', breaks = 30, probability = TRUE, main = 'Odotusarvo')
segments(ci95["mu", 1], 0, ci95["mu", 2], 0, lwd = 6)
abline(v = 100, lty = 2, col = 'red', lwd = 2)

n_sim <- length(sim$mu)
hist(y, col = 'pink', breaks = 30, probability = TRUE, main = 'Data w/ posterior predictive')
for(i in 1:100) {
  y_pred <- rnorm(n_sim, sim$mu, sim$sigma)
  dens <- density(y_pred)
  lines(dens, col = 'blue')
}
legend('topright', legend = 'Posterior predictive', lwd = 2, col = 'blue', inset = .05, bty = 'n')

hist(sim$sigma, col = 'skyblue', breaks = 30, probability = TRUE, xlim = c(12,35), 
     main = 'Keskihajonta')
segments(ci95["sigma", 1], 0, ci95["sigma", 2], 0, lwd = 6)
abline(v = 15, lty = 2, col = 'red', lwd = 2)

hist(y_pred, col = 'skyblue', breaks = 30, probability = TRUE, main = 'Posterior predictive')

# hist(sim$effect_size, col = 'skyblue', breaks = 30, probability = TRUE, 
#      main = 'Effect size')
# segments(ci95["effect_size", 1], 0, ci95["effect_size", 2], 0, lwd = 6)
# abline(v = 0, lty = 2, col = 'red', lwd = 2)

hist(sim$nu, col = 'skyblue', breaks = 100, probability = TRUE, 
     main = 'Vapausaste', xlim = c(0,100))
segments(ci95["nu", 1], 0, ci95["nu", 2], 0, lwd = 6)



