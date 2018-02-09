data {
  int<lower=0> n;
  real y[n];
  real y_mean;
  real<lower=0> y_sd;
}

parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> nu;
}

model {
  y ~ student_t(nu, mu, sigma);
  nu ~ exponential(1.0/30);
}
