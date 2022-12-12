// Parrot mimicry
// Simeon Q. Smeele
// Started: 10-11-2022
// Last modified: 08-12-2022
// Description: Model to estimate the effect of longevity. 

functions{
  vector merge_missing( int[] miss_indexes , vector x_obs , vector x_miss ) {
    int N = dims(x_obs)[1];
    int N_miss = dims(x_miss)[1];
    vector[N] merged;
    merged = x_obs;
    for ( i in 1:N_miss )
    merged[ miss_indexes[i] ] = x_miss[i];
    return merged;
  }
}
data{
  // Dimensions
  int N_obs;
  int N_species;
  int N_genera;
  int N_miss_brain;
  int N_miss_long;
  // One data point per species
  int genus[N_species];
  vector[N_species] stand_body;
  vector[N_species] stand_brain;
  int brain_missidx[N_miss_brain];
  vector[N_species] stand_long;
  int long_missidx[N_miss_long];
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
  vector[N_genera] z_genus_brain;
  vector[N_miss_brain] brain_impute;
  real<lower=0> sigma_genus_brain;
  real<lower=0> sigma_brain;
  // Impute longevity
  real a_long;
  real b_body_long;
  real b_brain_long;
  vector[N_miss_long] long_impute;
  vector[N_genera] z_genus_long;
  real<lower=0> sigma_genus_long;
  real<lower=0> sigma_long;
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
  vector[N_species] pred_brain;
  vector[N_species] mu_long;
  vector[N_species] mu_body;
  vector[N_species] brain_merge;
  vector[N_species] long_merge;
  // Brain model
  a_brain ~ normal(0, 0.5);
  z_genus_brain ~ normal(0, 1);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(2);
  sigma_genus_brain ~ exponential(2);
  // average brain is a function of genus and body size
  for(n in 1:N_species) mu_brain[n] = a_brain + 
  z_genus_brain[genus[n]] * sigma_genus_brain + 
  b_body_brain * stand_body[n];
  // predicted brain is only based on body size
  for(n in 1:N_species) pred_brain[n] = a_brain + 
  b_body_brain * stand_body[n];
  brain_merge = merge_missing(brain_missidx, to_vector(stand_brain), brain_impute);
  brain_merge ~ normal(mu_brain, sigma_brain);
  // Impute longevity
  a_long ~ normal(0, 0.5);
  z_genus_long ~ normal(0, 1);
  b_body_long ~ normal(0, 0.5);
  b_brain_long ~ normal(0, 0.5);
  sigma_long ~ exponential(2);
  sigma_genus_long ~ exponential(2);
  for(n in 1:N_species) mu_long[n] = a_long + 
    z_genus_long[genus[n]] * sigma_genus_long + 
    b_body_long * stand_body[n] +
    b_brain_long * (brain_merge[n] - pred_brain[n]);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(mu_long, sigma_long);
  // Main model
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(2);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(2);
  z_genus ~ normal(0, 1);
  b_long ~ normal(0, 1);
  b_body ~ normal(0, 1);
  b_brain ~ normal(0, 1);
  for(i in 1:N_obs) phi[i] = a_bar + 
    z_species[species[i]] * sigma_species +
    z_genus[genus[species[i]]] * sigma_genus + 
    z_greg[gregar[species[i]]] * sigma_greg +
    b_long * long_merge[species[i]] +
    b_body * stand_body[species[i]] + 
    b_brain * (brain_merge[species[i]] - pred_brain[species[i]]);
  for(i in 1:N_obs) quality[i] ~ ordered_logistic(phi[i], kappa);
}
