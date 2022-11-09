// Parrot mimicry
// Simeon Q. Smeele
// Started: 12-09-2022
// Last modified: 12-09-2022
// Description: Simple model explaining mimicry quality with species. 

data {
  int<lower=0> N_obs;
  int<lower=0> N_species;
  int<lower=0> N_genera;
  int n[N_obs];
  int species[N_obs];
  int genus[N_obs];
  vector[N_obs] log_time;
}

parameters {
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  real b_time;
}

model {
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(1);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(1);
  z_genus ~ normal(0, 1);
  b_time ~ normal(0, 1);
  n ~ poisson(exp(a_bar + 
  z_species[species] * sigma_species +
  z_genus[genus] * sigma_genus + 
  b_time * log_time));
}




