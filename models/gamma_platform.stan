data {
  //independent
  int<lower=0> n; //num of games
  vector[n] Xbox;
  vector[n] PS;
  
  //dependent
  vector<lower=0>[n] sales;
}


parameters {
  real intercept;
  real b_ps; //platform
  real b_xbox; //platform
  real<lower=0> phi; //variance
}
transformed parameters {
  vector[n] mu;
  vector[n] ni;
  vector[n] lambda;
  mu = (intercept + b_xbox * Xbox + b_ps * PS);
  
  ni = mu .* mu /phi;
  lambda = mu / phi;
  
}

model {
  
  //priors
  intercept ~ cauchy(0,2);
  b_ps ~ cauchy(0,2);
  b_xbox ~ cauchy(0,2);
  phi ~ cauchy(0,20);
  
  //model
  sales ~ gamma(ni, lambda);
}

