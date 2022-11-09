// Parrot mimicry
// Simeon Q. Smeele
// Started: 12-09-2022
// Last modified: 12-09-2022
// Description: Simple model explaining mimicry quality with species. 

data {
  int<lower=0> N_obs;
  int<lower=0> N_species;
  int n[N_obs];
  int species[N_obs];
}

parameters {
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
}

model {
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(0.5);
  z_species ~ normal(0, 1);
  n ~ poisson(exp(a_bar + z_species[species] * sigma_species));
}




