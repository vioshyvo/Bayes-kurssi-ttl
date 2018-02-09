# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Kahden otoksen "t-testi" 

##############################################################
# Posteriori kahden otoksen keskiarvolle (ja keskihajonnalle)

# lue aineisto ja pakkaa se listaan Stania varten
df <- read.csv( file="TwoGroupIQ.csv" )
y <- df$Score

smart_list <- list(n = length(y), y = y, y_mean = mean(y), y_sd = sd(y), group = as.numeric(df$Group))

library(rstan)
fit <- stan('smart_group.stan', iter = 1e4, data = smart_list)
fit

sim <- extract(fit)
str(sim)

par(mfrow = c(1,1))
plot(sim$sigma[ ,1], sim$sigma[ ,2], col = 'blue', pch = 20, cex = .3)
plot(sim$nu, sim$sigma[ ,2], col = 'blue', pch = 20, cex = .3)

sum <- summary(fit)$summary
ci95 <- sum[,c(4,8)]

par(mfrow = c(1,1))
pairs(fit, pars = c("mu", "sigma", "nu"))
