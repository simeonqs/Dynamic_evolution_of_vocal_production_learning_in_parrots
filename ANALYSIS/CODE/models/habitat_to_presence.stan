// Parrot mimicry
// Simeon Q. Smeele
// Started: 30-11-2022
// Last modified: 30-11-2022
// Description: Model to estimate the effect of habitat. 

data{
  int N_obs;
  int N_genera;
  int genus[N_obs];
  int habitat[N_obs];
  int mimic[N_obs];
}
parameters{
  // Main model
  real a_bar;
  vector[N_genera] z_genus;
  real<lower=0> sigma_genus;
  vector[3] z_hab;
  real<lower=0> sigma_hab;
}
model{
  // Declare
  vector[N_obs] p;
  // Main model
  a_bar ~ normal(0, 1);
  sigma_genus ~ exponential(2);
  z_hab ~ normal(0, 1);
  sigma_hab ~ exponential(2);
  z_genus ~ normal(0, 1);
  for(i in 1:N_obs){
    p[i] = a_bar + 
      z_genus[genus[i]] * sigma_genus + 
      z_hab[habitat[i]] * sigma_hab;
    p[i] = inv_logit(p[i]);
  } 
  mimic ~ binomial(1, p);
}
generated quantities{
  vector[3] a_hab;
  real cont_hab;
  a_hab = a_bar + z_hab * sigma_hab;
}
