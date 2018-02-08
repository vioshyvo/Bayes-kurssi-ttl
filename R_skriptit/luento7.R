# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Hello, Stan! - minimaalinen esimerkki Stanin kaytosta

# Lue kolikonheitot data frameen
df <- read.csv('finnish2e.csv', header = FALSE)
obs <- df$V1

n <- 5              # tarkastellaan 5 ensimmaista heittoa
y <- sum(obs[1:n])
coin_data <- list(y = y, n = n) # pakkaa aineisto listaan Stania varten

###################################################
# Sovita malli stanilla
library(rstan)
fit <- stan('coins.stan', iter = 1e4, data = coin_data)

fit
plot(fit)

sim <- extract(fit)

stan_ac(fit, 'theta')     # autocorrelations for theta
stan_dens(fit, 'theta')   # density estimate for theta
stan_trace(fit, 'theta')  # trace plot of 4 chains

library(shinystan)
shiny_obj <- launch_shinystan(fit)

##################################################
# Plottaa simuloitu posteriorijakauma
par(mfrow = c(1,1))
hist(sim$theta, col = 'skyblue', breaks = seq(0,1, by = .025), probability = TRUE,
     main = 'Analytical and simulated posterior')

# vertaa analyyttisesti ratkaistuun posteriorijakaumaan
theta <- seq(0,1,by=.001)
lines(theta, dbeta(theta, alpha + y, beta + n - y), col = 'mediumseagreen', lwd = 3)
legend('topleft', legend = 'true posterior', col = 'mediumseagreen', lwd = 3, bty = 'n', inset = .05)

#################################################     
# Simuloi posterioriennustejakaumasta
n_sim <- 1e4
m <- 10
y_pred <- rbinom(n_sim, size = m, prob = sim$theta)

df_bar <- barplot(table(y_pred) / n_sim, col = 'skyblue', ylim = c(0, .4),
                  main = 'Analytical and simulated posterior predictive')

# vertaa analyyttisesti ratkaistuun posterioriennustejakaumaan
alpha <- beta <- 1

if(m == 10 & nrow(df_bar) == m) df_bar <- cbind(df_bar, 12.7)
post_pred <- VGAM::dbetabinom.ab(0:m, m, alpha + y, beta + n - y)
lines(df_bar, post_pred, type = 'h', col = 'mediumseagreen', lwd = 3)
points(df_bar, post_pred, col = 'mediumseagreen', lwd = 3)
legend('topleft', legend = 'true posterior predictive', col = 'mediumseagreen', lwd = 3, bty = 'n', inset = .05)
