data {
  //independent
  int<lower=0> n; //num of games
  vector[n] isShooter;
  
  //dependent
  vector<lower=0>[n] sales;
}


parameters {
  real intercept;
  real b_isShooter; //platform
  real<lower=0> phi; //variance
}
transformed parameters {
  vector[n] mu;
  vector[n] ni;
  vector[n] lambda;
  mu = (intercept + b_isShooter * isShooter);
  
  ni = mu .* mu /phi;
  lambda = mu / phi;
  
}

model {
  
  //priors
  intercept ~ cauchy(0,2);
  b_isShooter ~ cauchy(0,2);
  phi ~ cauchy(0,20);
  
  //model
  sales ~ gamma(ni, lambda);
}

