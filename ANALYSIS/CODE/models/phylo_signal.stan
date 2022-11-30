// Parrot mimicry
// Simeon Q. Smeele
// Started: 30-08-2022
// Last modified: 30-11-2022
// Description: Test if we can get a phylogentic signal estimated. 

functions{

    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
}

data {
  int<lower=0> N_species; # number of observations
  int mimic[N_species]; # 0/1
  matrix[N_species,N_species] dmat; # normalised phylogenetic distance
}

parameters {
  real a_bar;
  real<lower=0> etasq;
  real<lower=0> rhosq;
  vector[N_species] q;
}

model {
  vector[N_species] p;
  vector[N_species] mu;
  matrix[N_species,N_species] SIGMA;
  a_bar ~ normal(0, 1);
  rhosq ~ exponential( 0.1 );
  etasq ~ exponential( 1 );
  SIGMA = cov_GPL2(dmat, etasq, rhosq, 0.01);
  for(n in 1:N_species) mu[n] = a_bar;
  q ~ multi_normal(mu, SIGMA);
  for(n in 1:N_species) p[n] = inv_logit(q[n]);
  mimic ~ binomial(1, p);
}
