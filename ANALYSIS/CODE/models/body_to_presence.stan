// Parrot mimicry
// Simeon Q. Smeele
// Started: 11-11-2022
// Last modified: 11-11-2022
// Description: Model to estimate the effect of body size. 

data{
  int N_obs;
  int N_genera;
  int genus[N_obs];
  vector[N_obs] stand_body;
  int mimic[N_obs];
}
parameters{
  // Main model
  real a_bar;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  real b_body;
}
model{
  // Declare
  vector[N_obs] p;
  // Main model
  a_bar ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_genus ~ normal(0, 1);
  b_body ~ normal(0, 1);
  for(i in 1:N_obs){
    p[i] = a_bar + 
      z_genus[genus[i]] * sigma_genus + 
      b_body * stand_body[i];
    p[i] = inv_logit(p[i]);
  } 
  mimic ~ binomial(1, p);
}
