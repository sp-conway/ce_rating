data {
  array[4] int<lower=0> N_att;
  array[4] int<lower=0> N_rep;
  
  matrix[N_att[1],3] laptops_att;
  matrix[N_att[2],3] microwaves_att;
  matrix[N_att[3],3] tv_att;
  matrix[N_att[4],3] washers_att;
  
  matrix[N_rep[1],3] laptops_rep;
  matrix[N_rep[2],3] microwaves_rep;
  matrix[N_rep[3],3] tv_rep;
  matrix[N_rep[4],3] washers_rep;
}

parameters {
  array[4] vector[3] real mu_attraction;
  array[4] vector[3] real mu_repulsion;
  
  // correlation mat in chol. form
  cholesky_factor_corr[3] omega_attraction;
  cholesky_factor_corr[3] omega_repulsion;

  // variances
  real<lower=0> s;
}
transformed parameters{
  vector[3] sigma = rep_vector(s, 3);  // Covariance matrix
  matrix[3,3] L_attraction = diag_pre_multiply(s,omega_attraction);
  matrix[3,3] L_repulsion = diag_pre_multiply(s,omega_repulsion);
}
model {
  s ~ cauchy(0,2.5);
  omega_attraction ~ lkj_corr_cholesky(1);
  omega_repulsion ~ lkj_corr_cholesky(1);
  for(i in 1:4){
    for(j in 1:3){
      mu_attraction[i,j] ~ normal(0,1);
      mu_repulsion[i,j] ~ normal(0,1);
    }
  }
  for(i in 1:N_att[1]){
    laptops_att[i,] ~ multi_normal_cholesky(mu_attraction[1],L_attraction);
  }
  for(i in 1:N_att[2]){
    microwaves_att[i,] ~ multi_normal_cholesky(mu_attraction[2],L_attraction);
  }
  for(i in 1:N_att[3]){
    tv_att[i,] ~ multi_normal_cholesky(mu_attraction[3],L_attraction);
  }
  for(i in 1:N_att[4]){
    washers_att[i,] ~ multi_normal_cholesky(mu_attraction[4],L_attraction);
  }
  
  for(i in 1:N_rep[1]){
    laptops_rep[i,] ~ multi_normal_cholesky(mu_repulsion[1],L_repulsion);
  }
  for(i in 1:N_rep[2]){
    microwaves_rep[i,] ~ multi_normal_cholesky(mu_repulsion[2],L_repulsion);
  }
  for(i in 1:N_rep[3]){
    tv_rep[i,] ~ multi_normal_cholesky(mu_repulsion[3],L_repulsion);
  }
  for(i in 1:N_rep[4]){
    washers_rep[i,] ~ multi_normal_cholesky(mu_repulsion[4],L_repulsion);
  }
  
}

generated quantities{
  // correlation matrix
  matrix [3,3] cor_attraction=multiply_lower_tri_self_transpose(omega_attraction);
  matrix [3,3] cor_repulsion=multiply_lower_tri_self_transpose(omega_repulsion);
}
