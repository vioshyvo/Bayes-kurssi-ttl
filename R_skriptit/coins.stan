data {
  int<lower=0> n;          // aineiston koko
  int<lower=0, upper=n> y; // onnistumisten maara n:ssa kokeessa
}

parameters {
  real<lower=0, upper=1> theta; // onnistumistodennakoisyys
}

model {
  theta ~ beta(1,1);
  y ~ binomial(n, theta);
}
