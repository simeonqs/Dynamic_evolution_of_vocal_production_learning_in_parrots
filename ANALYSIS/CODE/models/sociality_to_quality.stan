// Parrot mimicry
// Simeon Q. Smeele
// Started: 08-12-2022
// Last modified: 08-12-2022
// Description: Model to estimate the effect of sociality. 

data{
  // Dimensions
  int N_obs;
  int N_species;
  int N_genera;
  // One data point per species
  int genus[N_species];
  int gregar[N_species];
  // Multiple data points per species
  int quality[N_obs];
  int species[N_obs];
  // Alpha
  vector[3] alpha;
}
parameters{
  // Main model
  ordered[2] kappa;
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_greg;
  real<lower=0> sigma_greg;
}
model{
  // Declare
  vector[N_obs] phi;
  // Main model
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(2);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(2);
  z_genus ~ normal(0, 1);
  for(i in 1:N_obs) phi[i] = a_bar + 
    z_species[species[i]] * sigma_species +
    z_genus[genus[species[i]]] * sigma_genus + 
    z_greg[gregar[species[i]]] * sigma_greg;
  for(i in 1:N_obs) quality[i] ~ ordered_logistic(phi[i], kappa);
}
generated quantities{
  vector[2] a_greg;
  real cont_greg;
  a_greg = z_greg * sigma_greg;
  cont_greg = a_greg[2] - a_greg[1];
}
