// Parrot mimicry
// Simeon Q. Smeele
// Started: 11-11-2022
// Last modified: 11-11-2022
// Description: Model to estimate the effect of sociality. 

data{
  int N_obs;
  int N_genera;
  int genus[N_obs];
  int gregar[N_obs];
  int mimic[N_obs];
}
parameters{
  // Main model
  real a_bar;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  vector[2] z_greg;
  real<lower=0> sigma_greg;
}
model{
  // Declare
  vector[N_obs] p;
  // Main model
  a_bar ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_greg ~ normal(0, 1);
  sigma_greg ~ exponential(2);
  z_genus ~ normal(0, 1);
  for(i in 1:N_obs){
    p[i] = a_bar + 
      z_genus[genus[i]] * sigma_genus + 
      z_greg[gregar[i]] * sigma_greg;
    p[i] = inv_logit(p[i]);
  } 
  mimic ~ binomial(1, p);
}
