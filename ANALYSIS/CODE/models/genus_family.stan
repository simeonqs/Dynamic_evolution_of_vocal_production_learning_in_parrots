// Parrot mimicry
// Simeon Q. Smeele
// Started: 29-08-2022
// Last modified: 29-08-2022
// Description: Simple model explaining mimicry with genus. 

data {
  int<lower=0> N_species;
  int<lower=0> N_genus;
  int<lower=0> N_family;
  int mimic[N_species];
  int family[N_species];
  int genus[N_species];
}

parameters {
  real a_bar;
  vector[N_family] z_family;
  vector[N_genus] z_genus;
  real<lower=0> sigma_family;
  real<lower=0> sigma_genus;
}

model {
  vector[N_species] p;
  a_bar ~ normal(0, 1);
  sigma_family ~ exponential(1);
  sigma_genus ~ exponential(1);
  z_family ~ normal(0, 1);
  z_genus ~ normal(0, 1);
  for(n in 1:N_species){
    p[n] = a_bar + z_family[family[n]] * sigma_family + z_genus[genus[n]] * sigma_genus;
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




