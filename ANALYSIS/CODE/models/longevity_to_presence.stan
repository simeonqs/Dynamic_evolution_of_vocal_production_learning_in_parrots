// Parrot mimicry
// Simeon Q. Smeele
// Started: 11-11-2022
// Last modified: 11-11-2022
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
  int N_obs;
  int N_genera;
  int N_miss_brain;
  int N_miss_long;
  int genus[N_obs];
  vector[N_obs] stand_body;
  vector[N_obs] stand_brain;
  int brain_missidx[N_miss_brain];
  vector[N_obs] stand_long;
  int long_missidx[N_miss_long];
  int gregar[N_obs];
  int mimic[N_obs];
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
  vector[N_obs] p;
  vector[N_obs] mu_brain;
  vector[N_obs] pred_brain;
  vector[N_obs] mu_long;
  vector[N_obs] mu_body;
  vector[N_obs] brain_merge;
  vector[N_obs] long_merge;
  // Brain model
  a_brain ~ normal(0, 0.5);
  z_genus_brain ~ normal(0, 1);
  b_body_brain ~ normal(0, 0.5);
  sigma_brain ~ exponential(2);
  sigma_genus_brain ~ exponential(2);
  // average brain is a function of genus and body size
  for(n in 1:N_obs) mu_brain[n] = a_brain + 
  z_genus_brain[genus[n]] * sigma_genus_brain + 
  b_body_brain * stand_body[n];
  // predicted brain is only based on body size
  for(n in 1:N_obs) pred_brain[n] = a_brain + 
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
  for(n in 1:N_obs) mu_long[n] = a_long + 
    z_genus_long[genus[n]] * sigma_genus_long + 
    b_body_long * stand_body[n] +
    b_brain_long * (brain_merge[n] - pred_brain[n]);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(mu_long, sigma_long);
  // Main model
  a_bar ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(2);
  z_genus ~ normal(0, 1);
  b_long ~ normal(0, 1);
  b_body ~ normal(0, 1);
  b_brain ~ normal(0, 1);
  for(i in 1:N_obs){
    p[i] = a_bar + 
      z_genus[genus[i]] * sigma_genus + 
      z_greg[gregar[i]] * sigma_greg +
      b_long * long_merge[i] +
      b_body * stand_body[i] + 
      b_brain * (brain_merge[i] - pred_brain[i]);
    p[i] = inv_logit(p[i]);
  } 
  mimic ~ binomial(1, p);
}
generated quantities{
  vector[2] a_greg;
  real cont_greg;
  a_greg = z_greg * sigma_greg;
  cont_greg = a_greg[2] - a_greg[1];
}
