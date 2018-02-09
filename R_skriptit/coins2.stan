data {
  int<lower=0> n;               // heittojen maara
  int<lower=0> n_groups;        // ryhmien (aloitusmahdollisuuksien: kruuna tai klaava) maara, tassa 2 
  int<lower=0, upper=1> y[n];   // heittojen tulokset: 1 = kruuna, 0 = klaava 
  int<lower=0, upper=1> group[n]; // aloituspuolet: 1 = kruuna, 0 = klaava
}

parameters {
  real<lower=0, upper=1> theta[n_groups];
}

model {
  theta ~ beta(1,1);
  for(i in 1:n)
    y[i] ~ bernoulli(theta[group[i] + 1]); // huom. +1, Stanissa indeksit alkavat 1:sta
}

generated quantities {
  real theta_diff = theta[2] - theta[1];
}
