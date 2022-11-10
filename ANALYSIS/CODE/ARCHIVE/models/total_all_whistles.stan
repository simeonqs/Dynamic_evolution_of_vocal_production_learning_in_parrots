// Parrot mimicry
// Simeon Q. Smeele
// Started: 04-09-2022
// Last modified: 11-09-2022
// Description: Final model explaining mimicry with all variables, imputation and uncertainty. 
// This version includes sociality. 

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
  int N_miss_afr;
  int mimic[N_species];
  int genus[N_species];
  real stand_long[N_species];
  real se_long[N_species];
  int long_missidx[N_miss_long];
  real stand_brain[N_species];
  real se_brain[N_species];
  int brain_missidx[N_miss_brain];
  real stand_body[N_species];
  real se_body[N_species];
  real stand_afr[N_species];
  int afr_missidx[N_miss_afr];
  int sociality[N_species];
}

parameters {
  // Main model
  real a_bar;
  real b_long;
  real b_brain;
  real b_body;
  real b_afr;
  vector[N_genus] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_soc;
  real<lower=0> sigma_soc;
  // Brain model
  real a_brain;
  real b_body_brain;
  vector[N_genus] z_genus_brain;
  vector[N_miss_brain] brain_impute;
  vector[N_species] true_brain;
  real<lower=0> sigma_genus_brain;
  real<lower=0> sigma_brain;
  // Impute longevity
  real a_long;
  real b_body_long;
  real b_brain_long;
  vector[N_miss_long] long_impute;
  vector[N_genus] z_genus_long;
  vector[N_species] true_long;
  real<lower=0> sigma_genus_long;
  real<lower=0> sigma_long;
  // Impute body size
  real a_body;
  vector[N_genus] z_genus_body;
  vector[N_species] true_body;
  real<lower=0> sigma_genus_body;
  real<lower=0> sigma_body;
  // Impute afr
  real a_afr;
  real b_body_afr;
  vector[N_miss_afr] afr_impute;
  vector[N_genus] z_genus_afr;
  real<lower=0> sigma_genus_afr;
  real<lower=0> sigma_afr;
}

model {
  // Declare
  vector[N_species] p;
  vector[N_species] mu_brain;
  vector[N_species] pred_brain;
  vector[N_species] mu_long;
  vector[N_species] mu_body;
  vector[N_species] mu_afr;
  vector[N_species] brain_merge;
  vector[N_species] long_merge;
  vector[N_species] afr_merge;
  // Impute body size
  a_body ~ normal(0, 0.5);
  z_genus_body ~ normal(0, 1);
  sigma_body ~ exponential(1);
  sigma_genus_body ~ exponential(1);
  for(n in 1:N_species) mu_body[n] = a_body + z_genus_body[genus[n]] * sigma_genus_body;
  true_body ~ normal(mu_body, sigma_body);
  stand_body ~ normal(true_body, se_body);
  // Brain model
  a_brain ~ normal(0, 0.5);
  z_genus_brain ~ normal(0, 1);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(1);
  sigma_genus_brain ~ exponential(1);
  // average brain is a function of genus and body size
  for(n in 1:N_species) mu_brain[n] = a_brain + 
  z_genus_brain[genus[n]] * sigma_genus_brain + 
  b_body_brain * true_body[n];
  // predicted brain is only based on body size
  for(n in 1:N_species) pred_brain[n] = a_brain + 
  b_body_brain * true_body[n];
  true_brain ~ normal(mu_brain, sigma_brain);
  brain_merge = merge_missing(brain_missidx, to_vector(stand_brain), brain_impute);
  brain_merge ~ normal(true_brain, se_brain);
  // Impute longevity
  a_long ~ normal(0, 0.5);
  z_genus_long ~ normal(0, 1);
  b_body_long ~ normal(0, 0.5);
  b_brain_long ~ normal(0, 0.5);
  sigma_long ~ exponential(1);
  sigma_genus_long ~ exponential(1);
  for(n in 1:N_species) mu_long[n] = a_long + 
    z_genus_long[genus[n]] * sigma_genus_long + 
    b_body_long * true_body[n] +
    b_brain_long * (brain_merge[n] - pred_brain[n]);
  true_long ~ normal(mu_long, sigma_long);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(true_long, se_long);
  // Impute afr
  a_afr ~ normal(0, 0.5);
  b_body_afr ~ normal(0, 0.5);
  z_genus_afr ~ normal(0, 1);
  sigma_afr ~ exponential(1);
  sigma_genus_afr ~ exponential(1);
  for(n in 1:N_species) mu_afr[n] = a_afr + 
  z_genus_afr[genus[n]] * sigma_genus_afr +
  b_body_afr * true_body[n];
  afr_merge = merge_missing(afr_missidx, to_vector(stand_afr), afr_impute);
  afr_merge ~ normal(mu_afr, sigma_afr);
  // Mimicry model
  a_bar ~ normal(0, 1);
  b_long ~ normal(0, 0.5);
  b_brain ~ normal(0, 0.5);
  b_body ~ normal(0, 0.5);
  b_afr ~ normal(0, 0.5);
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
    b_body * true_body[n] +
    b_afr * afr_merge[n];
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




