data {
  int n_experiments;
  int<lower=0> N[n_experiments];
  int<lower=0> y[n_experiments];
}

parameters {
  real alpha[n_experiments];
  real mu;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0, upper=1> theta[n_experiments] = inv_logit(alpha);
}

model {
  mu ~ normal(1,1);
  sigma ~ normal(0,1);
  alpha ~ normal(mu, sigma);
  y ~ binomial_logit(N, alpha);
}
