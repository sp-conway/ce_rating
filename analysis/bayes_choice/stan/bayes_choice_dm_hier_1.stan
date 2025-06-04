// dirichlet-multinomial model, hierarchical
data {
  int<lower=0> S; // N subs
  int<lower=0> E; // N effects
  int<lower=0> D; // N distances
  int<lower=0> C; // N categories
  int<lower=0> H; // N t high
  int<lower=0> K; // N options
  array[S,E,D,C,H,K] int counts;
}

parameters {
  array[S,E,D,C,H] simplex[K] theta;
  array[E,D,C,H] vector<lower=0> simplex[K] alpha_prob;
  array[E,D,C,H] vector<lower=0> [1] multiplier; //fix this
}

transformed parameters{
  array[E,D,C,H] vector<lower=0> [K] alpha;
  
}


model {
  // group level effects
  for(i in 1:E){
    for(j in 1:D){
      for(k in 1:C){
        for(l in 1:H){
          alpha_prob[i,j,k,l] ~ dirichlet(1,1,1);
          multiplier[i,j,k,l] ~ uniform(.5,500);
          
           // move to transformed params
          alpha[i,j,k,l] = alpha_prob[i,j,k,l]*multiplier[i,j,k,l];
        }
      }
    }
  }
  
  // alpha prob - alpha normalized to probability, specified as dirichlet
  // multiplier / concentration - alpha multiplied by this. log norm or just uniform
  //
  
  // subject level effects
  // subject - as a prior, use the group level distributions
  for(i in 1:E){
    for(j in 1:D){
      for(k in 1:C){
        for(l in 1:H){
          for(m in 1:S){
            theta[i,j,k,l,m] ~ dirichlet(alpha[i,j,k,l]);
            counts[i,j,k,l,m,] ~ multinomial(theta[i,j,k,l]);
          }
        }
      }
    }
  }
  
  // for inference look at alphas
  // 3 alphas for every condition, could take that and get tdc probs out of these and do inference on that
  // alphas or alpha probs? - start from alphas, though alpha probs are easier to think about
  // on average how much do people out of this dirichlet pick each option
  
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

