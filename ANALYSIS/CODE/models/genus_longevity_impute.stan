// Parrot mimicry
// Simeon Q. Smeele
// Started: 04-09-2022
// Last modified: 04-09-2022
// Description: Simple model explaining mimicry with genus and longevity, includes imputation of missing
// longevity. 

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
  int N_miss_long;
  int mimic[N_species];
  int genus[N_species];
  real stand_long[N_species];
  int long_missidx[N_miss_long];
}

parameters {
  // Main model
  real a_bar;
  real b;
  vector[N_genus] z;
  real<lower=0> sigma_genus;
  // Impute longevity
  real mu_long;
  vector[N_miss_long] long_impute;
  real<lower=0> sigma_long;
}

model {
  // Declare
  vector[N_species] p;
  vector[N_species] long_merge;
  // Impute longevity
  sigma_long ~ exponential(1);
  mu_long ~ normal(0, 0.5);
  long_merge = merge_missing(long_missidx, to_vector(stand_long), long_impute);
  long_merge ~ normal(mu_long, sigma_long);
  // Main model
  a_bar ~ normal(0, 1);
  b ~ normal(0, 0.5);
  sigma_genus ~ exponential(1);
  z ~ normal(0, 1);
  for(n in 1:N_species){
    p[n] = a_bar + z[genus[n]] * sigma_genus + b * long_merge[n];
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




