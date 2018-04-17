data {
  int N;
  real X[N];
  real Y[N];
  int N_new;
  real X_new[N_new];
  real<lower=0> U;
}

parameters {
  real a;
  real b;
  real<lower=0> s_X;
  real<lower=0> s_Y;
  real<lower=22, upper=60> x_true[N];
}

model {
  s_X ~ student_t(4,0,0.5);
  for (n in 1:N) {
    target += log1p_exp(-log(U) + double_exponential_lpdf(X[n] | x_true[n], s_X));
    Y[n] ~ normal(a + b*x_true[n], s_Y);
  }
}

generated quantities {
  real Y_pred[N_new];
  for (n in 1:N_new) {
    Y_pred[n] = normal_rng(a + b*X_new[n], s_Y);
  }
}
