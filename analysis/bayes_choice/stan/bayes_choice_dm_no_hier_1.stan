// dirichlet-multinomial model, fully collapsed 
data {
  int<lower=0> E; // N effects
  int<lower=0> D; // N distances
  int<lower=0> C; // N categories
  int<lower=0> H; // N t high
  int<lower=0> K; // N options
  array[E,D,C,H,K] int counts;
}

parameters {
  array[E,D,C,H] vector<lower=0> [K] alpha;
  array[E,D,C,H] simplex[K] theta;
}


model {
  for(i in 1:E){
    for(j in 1:D){
      for(k in 1:C){
        for(l in 1:H){
          alpha[i,j,k,l] ~ lognormal(1,1);
          theta[i,j,k,l] ~ dirichlet(alpha[i,j,k,l]);
          counts[i,j,k,l,] ~ multinomial(theta[i,j,k,l]);
        }
      }
    }
  }
}

generated quantities{
  array[E,D,K] real theta_m;
  for (i in 1:E) {
    for (j in 1:D) {
      for (k in 1:K) {
        real sum_theta = 0;
        int count = 0;
        for (c in 1:C) {
          for (h in 1:H) {
            sum_theta += theta[i,j,c,h][k];
            count += 1;
          }
        }
        theta_m[i,j,k] = sum_theta / count;
      }
    }
  }
}

