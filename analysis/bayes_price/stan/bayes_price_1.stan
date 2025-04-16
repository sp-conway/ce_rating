data {
  int<lower=0> N;
  int<lower=0> n_cat;
  array[N, n_cat] vector[3] attraction;
  array[N, n_cat] vector[3] repulsion;
}

parameters {
  array[n_cat, 3] real mu_attraction;
  array[n_cat, 3] real mu_repulsion;
  
  // correlation mat in chol. form
  cholesky_factor_corr[3] omega_attraction;
  cholesky_factor_corr[3] omega_repulsion;

  // variances
  vector<lower=0>[3] s;
}
transformed parameters{
  // Covariance matrix
  matrix[3,3] S_attraction = diag_pre_multiply(s,omega_attraction);
  matrix[3,3] S_repulsion = diag_pre_multiply(s,omega_repulsion);
}
model {
  s ~ cauchy(0,2.5);
  omega_attraction ~ lkj_corr_cholesky(1);
  omega_repulsion ~ lkj_corr_cholesky(1);
  for(i in 1:n_cat){
    for(j in 1:3){
      mu_attraction[i,j] ~ normal(0,3);
      mu_repulsion[i,j] ~ normal(0,3);
    }
  }
  for(i in 1:N){
    for(j in 1:n_cat){
      attraction[i,j]~multi_normal_cholesky(to_vector(mu_attraction[j,]),S_attraction);
      repulsion[i,j]~multi_normal_cholesky(to_vector(mu_repulsion[j,]),S_repulsion);
    }
  }
}

generated quantities{
  // correlation matrix
  matrix [3,3] cor_attraction=multiply_lower_tri_self_transpose(omega_attraction);
  matrix [3,3] cor_repulsion=multiply_lower_tri_self_transpose(omega_repulsion);
}
