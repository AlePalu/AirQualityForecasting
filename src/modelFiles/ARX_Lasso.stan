data{
	int<lower = 0> N;       // number of obs
	int<lower = 0> K;       // number of covariates (including the intercept)
	real<lower = 0> Y[N];   // response vector
	matrix[N, K] X;		// covariates
	real<lower = 0> scale_s2;
}

parameters{
	vector[K] beta;
	real<lower = 0> lambda2;
	real<lower = 0> sigma2;
}

transformed parameters{
	vector[N] mu;
  	mu = X * beta;
}

model{
        // Likelihood:
        for (i in 1:N) {
             Y[i] ~ normal(mu[i], sigma2);
        }

	// Prior:
	// beta
	for (j in 1:K) {
	 	beta[j] ~ double_exponential(0.0, 1.0 / (pow(lambda2,0.5)));
	}
	lambda2 ~ exponential(0.1);
	sigma2 ~ inv_gamma(2., scale_s2);
}
