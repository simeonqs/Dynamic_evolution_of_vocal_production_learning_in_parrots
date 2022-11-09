// Parrot mimicry
// Simeon Q. Smeele
// Started: 03-09-2022
// Last modified: 03-09-2022
// Description: Simple model explaining mimicry with genus, longevity, residual brain size and body size. 

data {
  int<lower=0> N_species;
  int<lower=0> N_genus;
  int mimic[N_species];
  int genus[N_species];
  real stand_long[N_species];
  real stand_brain[N_species];
  real stand_body[N_species];
}

parameters {
  real a_bar;
  real b_long;
  real b_brain;
  real b_body;
  vector[N_genus] z;
  real<lower=0> sigma_genus;
}

model {
  vector[N_species] p;
  a_bar ~ normal(0, 1);
  b_long ~ normal(0, 0.5);
  b_brain ~ normal(0, 0.5);
  b_body ~ normal(0, 0.5);
  sigma_genus ~ exponential(1);
  z ~ normal(0, 1);
  for(n in 1:N_species){
    p[n] = a_bar + z[genus[n]] * sigma_genus + 
    b_long * stand_long[n] + 
    b_brain * stand_brain[n] +
    b_body * stand_body[n];
    p[n] = inv_logit(p[n]);
  } 
  mimic ~ binomial(1, p);
}




