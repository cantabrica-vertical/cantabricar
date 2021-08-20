data {
  int<lower=0> N;
  int<lower=0> y[N];
  int<lower=0> lambda_prior_alpha;
    int<lower=0> lambda_prior_beta;

}

parameters {
  real<lower=0> lambda;
}

model {
  lambda ~ gamma(lambda_prior_alpha, lambda_prior_beta);
  y ~ poisson(lambda);
}

generated quantities {

  // prior predictions
  real lambda_sim = gamma_rng(lambda_prior_alpha, lambda_prior_beta);

  real log_lik[N];
  real y_sim[N];
  int y_rep[N];

  // posterior predictions
  for (n in 1:N) {
    log_lik[n] = poisson_lpmf(y[n] | lambda);
    y_rep[n] = poisson_rng(lambda);
    y_sim[n] = poisson_rng(lambda_sim);
  }
}

