data {
  int<lower=1> J;                       // number of participants
  int<lower=1> N;                       // total number of trials
  array[N] int<lower=1, upper=J> subj;  // participant index for each trial
  vector[N] distance;                   // stimulus distance
  array[N] int<lower=0, upper=1> is_good;      // score outcome (1 good)
  array[N] int<lower=0, upper=1> behaviour;    // 1 if estimated rule == random
  array[J + 1] int<lower=1> start_idx;  // start index per participant (last = N+1)
  real<lower=0> step_slope;             // slope for smooth step
}

parameters {
  vector[J] alpha_G_raw;
  vector[J] alpha_B_raw;
  vector[J] beta_G_raw;
  vector[J] beta_B_raw;
  vector[J] gamma_raw;
  vector[J] threshold_raw;

  real mu_alpha_G;
  real<lower=0> sigma_alpha_G;
  real mu_alpha_B;
  real<lower=0> sigma_alpha_B;
  real mu_beta_G;
  real<lower=0> sigma_beta_G;
  real mu_beta_B;
  real<lower=0> sigma_beta_B;
  real mu_gamma;
  real<lower=0> sigma_gamma;
  real mu_threshold;
  real<lower=0> sigma_threshold;
}

transformed parameters {
  vector[J] alpha_G = mu_alpha_G + sigma_alpha_G * alpha_G_raw;
  vector[J] alpha_B = mu_alpha_B + sigma_alpha_B * alpha_B_raw;
  vector[J] beta_G  = mu_beta_G  + sigma_beta_G  * beta_G_raw;
  vector[J] beta_B  = mu_beta_B  + sigma_beta_B  * beta_B_raw;
  vector[J] gamma   = inv_logit(mu_gamma + sigma_gamma * gamma_raw);      // 0-1
  vector[J] threshold = mu_threshold + sigma_threshold * threshold_raw;   // unconstrained
}

model {
  // Hyperpriors
  mu_alpha_G ~ normal(0, 5);
  sigma_alpha_G ~ cauchy(0, 2.5);
  mu_alpha_B ~ normal(0, 5);
  sigma_alpha_B ~ cauchy(0, 2.5);
  mu_beta_G ~ normal(0, 5);
  sigma_beta_G ~ cauchy(0, 2.5);
  mu_beta_B ~ normal(0, 5);
  sigma_beta_B ~ cauchy(0, 2.5);
  mu_gamma ~ normal(0, 1);
  sigma_gamma ~ cauchy(0, 1);
  mu_threshold ~ normal(20, 10);
  sigma_threshold ~ cauchy(0, 5);

  // Subject level priors
  alpha_G_raw ~ normal(0, 1);
  alpha_B_raw ~ normal(0, 1);
  beta_G_raw  ~ normal(0, 1);
  beta_B_raw  ~ normal(0, 1);
  gamma_raw   ~ normal(0, 1);
  threshold_raw ~ normal(0, 1);

  // Likelihood
  for (j in 1:J) {
    real z;
    real bias;
    int s = start_idx[j];
    int e = start_idx[j + 1] - 1;

    real pred_center = inv_logit(step_slope * (threshold[j] - distance[s]));
    real err = abs(pred_center - is_good[s]);
    if (is_good[s] == 0) {
      z = alpha_B[j] * err;
      bias = beta_B[j];
    } else {
      z = alpha_G[j] * err;
      bias = beta_G[j];
    }
    target += bernoulli_lpmf(behaviour[s] | inv_logit(z + bias));

    for (n in (s + 1):e) {
      real pred_c = inv_logit(step_slope * (threshold[j] - distance[n]));
      real e_n = abs(pred_c - is_good[n]);
      if (is_good[n] == 0) {
        z = alpha_B[j] * e_n + gamma[j] * z;
        bias = beta_B[j];
      } else {
        z = alpha_G[j] * e_n + gamma[j] * z;
        bias = beta_G[j];
      }
      target += bernoulli_lpmf(behaviour[n] | inv_logit(z + bias));
    }
  }
}

generated quantities {
  vector[N] log_lik;
  {
    for (j in 1:J) {
      real z;
      real bias;
      int s = start_idx[j];
      int e = start_idx[j + 1] - 1;

      real pred_center = inv_logit(step_slope * (threshold[j] - distance[s]));
      real err = abs(pred_center - is_good[s]);
      if (is_good[s] == 0) {
        z = alpha_B[j] * err;
        bias = beta_B[j];
      } else {
        z = alpha_G[j] * err;
        bias = beta_G[j];
      }
      log_lik[s] = bernoulli_lpmf(behaviour[s] | inv_logit(z + bias));

      for (n in (s + 1):e) {
        real pred_c = inv_logit(step_slope * (threshold[j] - distance[n]));
        real e_n = abs(pred_c - is_good[n]);
        if (is_good[n] == 0) {
          z = alpha_B[j] * e_n + gamma[j] * z;
          bias = beta_B[j];
        } else {
          z = alpha_G[j] * e_n + gamma[j] * z;
          bias = beta_G[j];
        }
        log_lik[n] = bernoulli_lpmf(behaviour[n] | inv_logit(z + bias));
      }
    }
  }
}

