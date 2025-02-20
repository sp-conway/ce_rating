data {
  // scalars
  int<lower=0> N; // number of data points 
  
  int<lower=0> J; // number of matrices
  
  // array of all indices
  array[N] int set;
  
  // trial-by-trial matrix of Target-Competitor-Decoy values
  matrix[N,3] X;
  
}

parameters {
  // variances
  vector<lower=0>[3] s;
  
  // freely estimating mu
  vector[3] mu; 
  
  // vcv weighting param
  real<lower=0,upper=1> rho;
  
  // global and local cor mats
  corr_matrix[3] omega_global;
  corr_matrix[3] omega_local[J];
}

transformed parameters{
  corr_matrix[3] Omega[J];
  for(i in 1:J){
    Omega[i]=rho*omega_global +(1-rho)*omega_local[i];
  }
}


model {
  // fixed effects
  mu ~ normal(0,5);
  
  // correlation
  for(i in 1:J){
    omega_local[i] ~ lkj_corr(1);
  }
  omega_global ~ lkj_corr(1);
  
  // standard deviations
  s ~ cauchy(0,2.5);
  
  rho~beta(2,2);
  
  // ACTUAL DATA
  for(i in 1:N){
    X[i,] ~ multi_normal(mu,
                         quad_form_diag(Omega[set[i]],s));
  }
}

