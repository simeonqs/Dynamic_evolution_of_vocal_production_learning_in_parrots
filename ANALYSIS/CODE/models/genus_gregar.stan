// Parrot mimicry
// Simeon Q. Smeele
// Started: 03-09-2022
// Last modified: 03-09-2022
// Description: Simple model explaining mimicry with genus and gregariousness. 

data {
  int<lower=0> N_species;
  int<lower=0> N_genus;
  int mimic[N_species];
  int genus[N_species];
  int gregar[N_species];
}

parameters {
  real a_bar;
  vector[N_genus] z_genus;
  vector[2] z_greg;
  real<lower=0> sigma_genus;
  real<lower=0> sigma_greg;
}

model {
  vector[N_species] p;
  a_bar ~ normal(0, 1);
  sigma_genus ~ exponential(1);
  sigma_greg ~ exponential(1);
  z_genus ~ normal(0, 1);
  z_greg ~ normal(0, 1);
  for(n in 1:N_species){
    p[n] = a_bar + z_genus[genus[n]] * sigma_genus + z_greg[gregar[n]] * sigma_greg;
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




