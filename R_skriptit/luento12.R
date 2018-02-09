# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Hierarkkinen malli 

###############################################
# install.packages('rstan')
# install.packages('VGAM)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(VGAM)

# lue aineisto ja pakkaa se listaan Stania varten
# n_experiments = pelaajien maara
# N = lyontivuorojen maarat
# y = osujien maarat
bball <- read.csv('baseball75.csv')
baseball_list <- list(n_experiments = nrow(bball), N = bball$AB, y = bball$Hits)

n_sim <- 10000        
n_players <- nrow(bball)


###############################################
# No pooling - malli
m1 <- list()

# variables for each parameter of posteriors for theta_1, ... theta_18
m1$y_prob <- m1$alpha <- m1$beta <- numeric(n_players)

m1$y <- bball$Hits
m1$n <- bball$AB
m1$alpha <- m1$y + 1
m1$beta <- m1$n - m1$y + 1

# posterior quantiles
m1$posterior_medians <- qbeta(.5, m1$alpha, m1$beta)
m1$q1 <- qbeta(.25, m1$alpha, m1$beta)
m1$q2 <- qbeta(.75, m1$alpha, m1$beta)

# posterior sample
m1$theta <- matrix(numeric(n_sim * n_players), ncol = n_players) 
for(i in 1:n_players) 
  m1$theta[ ,i] <- rbeta(n_sim, m1$alpha[i], m1$beta[i])

# code for all plots can be found at the end of file

###############################################
# Complete pooling - malli

# counts y_j are generated from the same binomial distribution bin(n_j, theta) for each player
m2 <- list()

m2$n <- sum(bball$AB)
m2$y <- sum(bball$Hits)

# posterior parameter
m2$alpha <- m2$y + 1
m2$beta <- m2$n - m2$y + 1

# posterior quantiles
m2$posterior_medians <- rep(qbeta(.5, m2$alpha, m2$beta), n_players)
m2$q1 <- rep(qbeta(.25, m2$alpha, m2$beta), n_players)
m2$q2 <- rep(qbeta(.75, m2$alpha, m2$beta), n_players)

# posterior sample
m2$theta <- matrix(rbeta(n_sim * n_players, m2$alpha, m2$beta), nrow = n_sim)

###########################################################
# Hierarkkinen malli

# remember to make sure that you have the needed .stan-files 
# within your working directory
m3 <- list()
(m3$fit <- stan('hierarchical.stan', data = baseball_list, iter = 1e4, control = list(adapt_delta = .99, stepsize = .01))) 
m3$sim <- extract(m3$fit)

# posterior quantiles
m3$posterior_medians <- apply(m3$sim$theta, 2, median)
m3$q1 <- apply(m3$sim$theta, 2, quantile, probs = .25)
m3$q2 <- apply(m3$sim$theta, 2, quantile, probs = .75)


###########################################################
# Hierarkkinen malli logit-linkilla

m4 <- list()

# argument control = list(adapt_delta = .99, stepsize = .01) is used to help the sampler perform correctly
(m4$fit <- stan('hierarchical_log.stan', data = baseball_list, iter = 1e4, control = list(adapt_delta = .99, stepsize = .01))) 
m4$sim <- extract(m4$fit)

# posterior quantiles
m4$posterior_medians <- apply(m4$sim$theta, 2, median)
m4$q1 <- apply(m4$sim$theta, 2, quantile, probs = .25)
m4$q2 <- apply(m4$sim$theta, 2, quantile, probs = .75)


#########################################################
# Simuloi posterioriennustejakaumasta

# generating samples from the posterior predictive distribution 
sim_y <- function(theta, m) {
  n_iter <- nrow(theta)
  n_players <- ncol(theta) 
  y_pred <- matrix(numeric(n_iter * n_players), ncol = n_players)
  for(i in 1:n_players) 
    y_pred[ ,i] <- rbinom(n_iter, size = m[i], prob = theta[ ,i]) / m[i]
  y_pred
}

m1$y_pred <- sim_y(m1$theta, bball$RemainingAB)
m2$y_pred <- sim_y(m2$theta, bball$RemainingAB)
m3$y_pred <- sim_y(m3$sim$theta, bball$RemainingAB)
m4$y_pred <- sim_y(m4$sim$theta, bball$RemainingAB)

#########################################################
# Vertaile testiaineistolla

# log predictive density for RemainingHits
predict_y <- function(model, y, m) {
  n_iter <- nrow(model$sim$theta)
  n_players <- ncol(model$sim$theta) 
  y_probs <-  matrix(numeric(n_iter * n_players), ncol = n_players)
  for(i in 1:n_players) 
    y_probs[ ,i] <- dbinom(y[i], size = m[i], prob = model$sim$theta[ ,i])
  
  prob <- colMeans(y_probs)
}

# log-posterioriennustejakauman arvot eri malleille
sum(log(dbetabinom.ab(bball$RemainingHits, bball$RemainingAB, m1$alpha, m1$beta))) # no pooling
sum(log(dbetabinom.ab(bball$RemainingHits, bball$RemainingAB, m2$alpha, m2$beta))) # complete pooling
sum(log(predict_y(m3, bball$RemainingHits, bball$RemainingAB)))                    # hierarchical 
sum(log(predict_y(m4, bball$RemainingHits, bball$RemainingAB)))                    # hierarchical with logit link



####################################################################
# plots

# posterior medians and quantiles

plot_medians <- function(m1, h_line, ...) {
  plot(bball$Hits/bball$AB, m1$posterior_medians, pch = 20, col = 'blue', asp = 1, cex = 1.2,
       ylab = expression(theta[j]), xlab = expression(y[j]/n[j]), ...)
  abline(a = 0, b = 1, col = 'darkblue', lwd = 2)
  abline(h = h_line, col = 'darkred', lwd = 2)
  arrows(bball$Hits/bball$AB, m1$q1, bball$Hits/bball$AB, m1$q2, length=0.05, angle=90, code=3, lwd = 2)
}

par(mfrow = c(2,2))
plot_medians(m1, median(m2$theta), ylim = c(.1,.5),  main = 'No pooling')
plot_medians(m2, median(m2$theta), ylim = c(.15,.4),  main = 'Complete pooling')
plot_medians(m3, median(m2$theta), ylim = c(.15,.4),  main = 'Hierarchical model')
plot_medians(m4, median(m2$theta), ylim = c(.15,.4),  main = 'Hierarchical model with logit link')
# dev.off()


##############################################################
# boxplots for the predictions

plot_pred <- function(m1, ...) {
  boxplot(m1$y_pred, col = 'skyblue', xlab = 'Player', ylab = 'Batting avg.', ...)
  points(bball$Hits / bball$AB, col = 'red', pch = 17, lwd = 2)
  points(bball$RemainingHits / bball$RemainingAB, col = 'darkblue', pch = 16, lwd = 2)
  legend('topright', legend = c('First 45', 'Rest of the season'), pch = c(17,16), col = c('red', 'darkblue'), inset = .02)
}

# par(mfrow = c(4,1))
plot_pred(m1, ylim = c(0,.7), main = 'No pooling')
plot_pred(m2, ylim = c(0,.7), main = 'Complete pooling')
plot_pred(m3, ylim = c(0,.6), main = 'Hierarchical model')
plot_pred(m4, ylim = c(0,.6), main = 'Hierarchical model with logit link')
# dev.off()

