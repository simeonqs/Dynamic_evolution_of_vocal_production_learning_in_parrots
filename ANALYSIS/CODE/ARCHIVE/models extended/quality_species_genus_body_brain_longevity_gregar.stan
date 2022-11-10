// Parrot mimicry
// Simeon Q. Smeele
// Started: 14-09-2022
// Last modified: 18-09-2022
// Description: Simple model explaining mimicry quality with all variables. 

data{
  // Dimensions
  int<lower=0> N_obs;
  int<lower=0> N_species;
  int<lower=0> N_genera;
  // One data point per species
  int genus[N_species];
  vector[N_species] stand_body;
  vector[N_species] stand_brain;
  vector[N_species] stand_long;
  int gregar[N_species];
  // Multiple data points per species
  int quality[N_obs];
  int species[N_obs];
  // Alpha
  vector[3] alpha;
}
parameters{
  // Brain model
  real a_brain;
  real b_body_brain;
  real<lower=0> sigma_brain;
  // Main model
  ordered[2] kappa;
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_greg;
  real<lower=0> sigma_greg;
  real b_long;
  real b_body;
  real b_brain;
}
model{
  // Declare
  vector[N_obs] phi;
  vector[N_species] mu_brain;
  // Brain model - creates a mu_brain per species
  a_brain ~ normal(0, 0.5);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(1);
  for(i in 1:N_species) mu_brain[i] = a_brain + b_body_brain * stand_body[i];
  stand_brain ~ normal(mu_brain, sigma_brain);
  // Main model
  kappa ~ normal(0, 1.5);
  a_bar ~ normal(0, 2);
  sigma_species ~ exponential(1);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(1);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(1);
  z_genus ~ normal(0, 1);
  b_long ~ normal(0, 1);
  b_body ~ normal(0, 1);
  b_brain ~ normal(0, 1);
  for(i in 1:N_obs) phi[i] = a_bar + 
  z_species[species[i]] * sigma_species +
  z_genus[genus[species[i]]] * sigma_genus + 
  z_greg[gregar[species[i]]] * sigma_greg +
  b_long * stand_long[species[i]] +
  b_body * stand_body[species[i]] + 
  b_brain * (stand_brain[species[i]] - mu_brain[species[i]]);
  for(i in 1:N_obs) quality[i] ~ ordered_logistic(phi[i], kappa);
}
generated quantities{
  vector[2] a_greg;
  a_greg = z_greg * sigma_greg;
}
