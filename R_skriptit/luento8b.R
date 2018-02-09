# Bayes-paattely Tyoterveyslaitos 8.-9.2.2018
# Malli ristiintaulukolle

# Lue kolikonheitot data frameen
df <- read.csv('finnish2e.csv', header = FALSE)
names(df)[1] <- 'y'
n <- nrow(df)
df$start <- rep(c(1,0), n / 2)

t <- table(df$y, df$start, dnn = c('result', 'start'))
prop.table(t,2)

n1 <- sum(df$start == 0)
n2 <- sum(df$start == 1)
y1 <- sum(df$y[df$start == 0])
y2 <- sum(df$y[df$start == 1])

coin_list <- list(n = c(n1, n2), y = c(y1, y2))

fit <- stan('coins_group.stan', iter = 1e4, data = coin_list)
plot(fit)

sim <- extract(fit)
par(mfrow = c(2,2))
hist(sim$theta[,1], col = 'skyblue', breaks = 50, probability = TRUE)
hist(sim$theta[,2], col = 'skyblue', breaks = 50, probability = TRUE)
hist(sim$theta_diff, col = 'skyblue', breaks = 50, probability = TRUE)
abline(v = 0, lty = 2, col = 'red')

# P(theta_2 - theta_1 > 0 | y)
mean(sim$theta_diff > 0)

pairs(fit, pars = 'theta')
