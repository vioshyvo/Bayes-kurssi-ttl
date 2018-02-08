# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Ennustaminen posterioriennustejakauman avulla

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

# install.packages('VGAM')
library(VGAM)

#########################################################################
# Posterioriennustejakauman ja plug-in - estimaatin kuvaajat 

m <- 10
par(mfrow = c(1,1))
plot(0:m, dbetabinom.ab(0:m, m, alpha_n, beta_n), type = 'h', lwd = 3,
     col = 'mediumseagreen', ylab = 'probability', xlab = expression(tilde(y)), ylim = c(0,1))
points(0:m, dbetabinom.ab(0:m, m, alpha_n, beta_n), lwd = 3, col = 'mediumseagreen')
points((0:m)- .15, dbinom(0:m, size = m, prob = y / n), lwd = 3, col = 'violet')
lines((0:m)- .15, dbinom(0:m, size = m, prob = y / n), lwd = 3, type = 'h', col = 'violet')

legend('topleft', inset = .02, 
       legend = c(expression(paste('plug-in estimate p(', tilde(y), '|', hat(theta), ')')),
                  expression(paste('posterior predictive p(', tilde(y), '|', y, ')'))),
       col = c('violet', 'mediumseagreen'), lwd = 3, bty = 'n')

#########################################################################
# Posterioriennustejakauma simuloimalla

n_sim <- 1e4
theta_sim <- rbeta(n_sim, alpha_n, beta_n)
y_tilde <- rbinom(n_sim, size = m, prob = theta_sim)
df_bar <- barplot(table(y_tilde) / n_sim, col = 'skyblue', ylim = c(0,.4), 
                  main = "Analytical and simulated posterior predictive" )
lines(df_bar, dbetabinom.ab(0:m, m, alpha_n, beta_n), type = 'h', lwd = 3, col = 'mediumseagreen')
if(m == 10 & nrow(df_bar) == m) df_bar <- cbind(df_bar, 12.7)
points(df_bar, dbetabinom.ab(0:m, m, alpha_n, beta_n), lwd = 3, col = 'mediumseagreen')
legend('topleft', legend = 'analytical posterior predictive', col = 'mediumseagreen', bty = 'n', lwd = 2, inset = .05)


#########################################################################
# Posterioriennustejakaumat otoskoolla n = 5 - 250 

par(mfrow = c(3,2))
n_all <- c(5,10,25,50,100,250)
for(n in n_all) {
  y <- sum(obs[1:n])
  alpha_n <- alpha + y
  beta_n <- beta + n - y
  
  plot(0:m, dbetabinom.ab(0:m, m, alpha_n, beta_n), type = 'h', lwd = 3,
       col = 'mediumseagreen', ylab = 'probability', xlab = expression(tilde(y)), ylim = c(0,.3), main = paste('n =', n))
  points(0:m, dbetabinom.ab(0:m, m, alpha_n, beta_n), lwd = 3, col = 'mediumseagreen')
  points((0:m)- .15, dbinom(0:m, size = m, prob = y / n), lwd = 3, col = 'violet')
  lines((0:m)- .15, dbinom(0:m, size = m, prob = y / n), lwd = 3, type = 'h', col = 'violet')
  
  legend('topleft', inset = .02, 
         legend = c(expression(paste('plug-in estimate p(', tilde(y), '|', hat(theta), ')')),
                    expression(paste('posterior predictive p(', tilde(y), '|', y, ')'))),
         col = c('violet', 'mediumseagreen'), lwd = 3, bty = 'n')
  cat('n = ', n, '\n')
  cat(round(dbinom(0:m, size = m, prob = y / n), 3), '\n')
  cat(round(dbetabinom.ab(0:m, m, alpha_n, beta_n), 3), '\n\n')
}

