// Parrot mimicry
// Simeon Q. Smeele
// Started: 21-09-2022
// Last modified: 21-09-2022
// Description: Model explaining n mimics by species, genus, longevity, sociality, brain size and body size. 
// Includes imputation. 

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
  int n[N_obs];
  int species[N_obs];

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
  real a_bar;
  vector[N_species] z_species;
  real<lower=0> sigma_species;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_greg;
  real<lower=0> sigma_greg;
  real b_long;
  real b_brain;
  real b_body;
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
  sigma_brain ~ exponential(1);
  sigma_genus_brain ~ exponential(1);
  // average brain is a function of genus and body size
  for(i in 1:N_species) mu_brain[i] = a_brain + 
  z_genus_brain[genus[i]] * sigma_genus_brain + 
  b_body_brain * stand_body[i];
  // predicted brain is only based on body size
  for(i in 1:N_species) pred_brain[i] = a_brain + 
  b_body_brain * stand_body[i];
  brain_merge = merge_missing(brain_missidx, to_vector(stand_brain), brain_impute);
  brain_merge ~ normal(mu_brain, sigma_brain);
  // Impute longevity
  // priors
  a_long ~ normal(0, 0.5);
  z_genus_long ~ normal(0, 1);
  b_body_long ~ normal(0, 0.5);
  b_brain_long ~ normal(0, 0.5);
  sigma_long ~ exponential(1);
  sigma_genus_long ~ exponential(1);
  // model
  for(i in 1:N_species) mu_long[i] = a_long + 
    z_genus_long[genus[i]] * sigma_genus_long + 
    b_body_long * stand_body[i] +
    b_brain_long * (brain_merge[i] - pred_brain[i]);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(mu_long, sigma_long);
  // Main model
  // priors
  a_bar ~ normal(2, 2);
  sigma_species ~ exponential(1);
  z_species ~ normal(0, 1);
  sigma_genus ~ exponential(1);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(1);
  z_genus ~ normal(0, 1);
  b_long ~ normal(0, 1);
  b_brain ~ normal(0, 1);
  b_body ~ normal(0, 1);
  // model
  n ~ poisson(exp(a_bar + 
  z_species[species] * sigma_species +
  z_genus[genus[species]] * sigma_genus + 
  z_greg[gregar[species]] * sigma_greg +
  b_long * long_merge[species] +
  b_brain * (brain_merge[species] - pred_brain[species]) +
  b_body * stand_body[species]
  ));
}
generated quantities{
  vector[2] a_greg;
  a_greg = z_greg * sigma_greg;
}
