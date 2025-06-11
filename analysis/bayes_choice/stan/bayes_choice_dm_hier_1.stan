// dirichlet-multinomial model, hierarchical
data {
  int<lower=0> P; // N ppt
  int<lower=0> E; // N effects
  int<lower=0> D; // N distances
  int<lower=0> C; // N categories
  int<lower=0> H; // N t high
  int<lower=0> O; // N options
  array[S,E,D,C,H,K] int counts;
}

parameters {
  array[P,E,D,C,H] simplex[O] theta;
  array[E,D,C,H] vector<lower=0> simplex[O] alpha_prob;
  array[E,D,C,H] vector<lower=0> [1] multiplier; 
}

transformed parameters{
  array[E,D,C,H] vector<lower=0> [O] alpha;
  
}


model {
  // group level effects
  for(ee in 1:E){ // e is reserved keyword in stan
    for(d in 1:D){
      for(c in 1:C){
        for(h in 1:H){
          alpha_prob[ee,d,c,h] ~ dirichlet(1,1,1);
          multiplier[ee,d,c,h] ~ uniform(.5,500);
          
           // move to transformed params
          alpha[ee,d,c,h] = alpha_prob[ee,d,c,h]*multiplier[ee,d,c,h];
        }
      }
    }
  }
  
  // alpha prob - alpha normalized to probability, specified as dirichlet
  // multiplier / concentration - alpha multiplied by this. log norm or just uniform
  //
  
  // subject level effects
  // subject - as a prior, use the group level distributions
  for(p in 1:P){
    for(ee in 1:E){
      for(d in 1:D){
        for(c in 1:C){
          for(h in 1:H){
            theta[p,ee,d,c,h] ~ dirichlet(alpha[ee,d,c,h]);
            counts[p,ee,d,c,h] ~ multinomial(theta[ee,d,c,h]);
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
