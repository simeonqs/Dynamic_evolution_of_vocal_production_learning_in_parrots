// Parrot mimicry
// Simeon Q. Smeele
// Started: 04-09-2022
// Last modified: 12-09-2022
// Description: Final model explaining mimicry with all variables, imputation and uncertainty. 
// This version includes sociality. 
// This version has no afr. 

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
data {
  int N_species;
  int N_genus;
  int N_miss_brain;
  int N_miss_long;
  int mimic[N_species];
  int genus[N_species];
  real stand_long[N_species];
  int long_missidx[N_miss_long];
  real stand_brain[N_species];
  int brain_missidx[N_miss_brain];
  real stand_body[N_species];
  int sociality[N_species];
}

parameters {
  // Main model
  real a_bar;
  real b_long;
  real b_brain;
  real b_body;
  vector[N_genus] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_soc;
  real<lower=0> sigma_soc;
  // Brain model
  real a_brain;
  real b_body_brain;
  vector[N_genus] z_genus_brain;
  vector[N_miss_brain] brain_impute;
  real<lower=0> sigma_genus_brain;
  real<lower=0> sigma_brain;
  // Impute longevity
  real a_long;
  real b_body_long;
  real b_brain_long;
  vector[N_miss_long] long_impute;
  vector[N_genus] z_genus_long;
  real<lower=0> sigma_genus_long;
  real<lower=0> sigma_long;
  // Impute body size
  real a_body;
  vector[N_genus] z_genus_body;
  real<lower=0> sigma_genus_body;
  real<lower=0> sigma_body;
}

model {
  // Declare
  vector[N_species] p;
  vector[N_species] mu_brain;
  vector[N_species] pred_brain;
  vector[N_species] mu_long;
  vector[N_species] mu_body;
  vector[N_species] brain_merge;
  vector[N_species] long_merge;
  // Impute body size
  a_body ~ normal(0, 0.5);
  z_genus_body ~ normal(0, 1);
  sigma_body ~ exponential(1);
  sigma_genus_body ~ exponential(1);
  for(n in 1:N_species) mu_body[n] = a_body + z_genus_body[genus[n]] * sigma_genus_body;
  stand_body ~ normal(mu_body, sigma_body);
  // Brain model
  a_brain ~ normal(0, 0.5);
  z_genus_brain ~ normal(0, 1);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(1);
  sigma_genus_brain ~ exponential(1);
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
  sigma_long ~ exponential(1);
  sigma_genus_long ~ exponential(1);
  for(n in 1:N_species) mu_long[n] = a_long + 
    z_genus_long[genus[n]] * sigma_genus_long + 
    b_body_long * stand_body[n] +
    b_brain_long * (brain_merge[n] - pred_brain[n]);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(mu_long, sigma_long);
  // Mimicry model
  a_bar ~ normal(0, 1);
  b_long ~ normal(0, 0.5);
  b_brain ~ normal(0, 0.5);
  b_body ~ normal(0, 0.5);
  sigma_genus ~ exponential(1);
  z_genus ~ normal(0, 1);
  sigma_soc ~ exponential(1);
  z_soc ~ normal(0, 1);
  for(n in 1:N_species){
    p[n] = a_bar + 
    z_genus[genus[n]] * sigma_genus + 
    z_soc[sociality[n]] * sigma_soc +
    b_long * long_merge[n] + 
    b_brain * (brain_merge[n] - pred_brain[n]) +
    b_body * stand_body[n];
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




