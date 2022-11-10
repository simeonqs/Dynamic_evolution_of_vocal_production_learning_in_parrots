// Parrot mimicry
// Simeon Q. Smeele
// Started: 20-09-2022
// Last modified: 20-09-2022
// Simple model explaining mimicry quality with species, genus, longevity, sociality and 
// brain size. 

data {
  // Dimensions
  int<lower=0> N_obs;
  int<lower=0> N_species;
  int<lower=0> N_genera;
  // Brain model - one data point per species
  vector[N_species] stand_body_short;
  vector[N_species] stand_brain_short;
  int genus_short[N_species];
  // Main model - multiple data points per species
  int n[N_obs];
  int species[N_obs];
  int genus[N_obs];
  vector[N_obs] stand_long;
  vector[N_obs] stand_body;
}

parameters {
  // Brain model
  real a_brain;
  real b_body_brain;
  real<lower=0> sigma_brain;
  // Main model
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  real b_long;
  real b_body;
  real b_brain;
}

model {
  // Declare
  vector[N_species] mu_brain;
  // Brain model - creates a mu_brain per species
  a_brain ~ normal(0, 0.5);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(1);
  for(i in 1:N_species) mu_brain[i] = a_brain + b_body_brain * stand_body_short[i];
  stand_brain_short ~ normal(mu_brain, sigma_brain);
  // Main model
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(1);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(1);
  z_genus ~ normal(0, 1);
  b_long ~ normal(0, 1);
  b_body ~ normal(0, 1);
  b_brain ~ normal(0, 1);
  n ~ poisson(exp(a_bar + 
  z_species[species] * sigma_species +
  z_genus[genus] * sigma_genus + 
  b_long * stand_long +
  b_body * stand_body + 
  b_brain * (stand_brain_short[species] - mu_brain[species]) // need the species bit to match size
  ));
}




