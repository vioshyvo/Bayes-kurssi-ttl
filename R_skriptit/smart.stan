data {
  int<lower=0> n;
  real y[n];
  real y_mean;
  real<lower=0> y_sd;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
  mu ~ normal(y_mean, 10 * y_sd);
  sigma ~ uniform(y_sd / 1000, y_sd * 1000);
}

generated quantities {
  real effect_size = (mu - 100) / sigma;
}
