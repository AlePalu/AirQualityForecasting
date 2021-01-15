data{
        int<lower = 0> N;       // number of obs
        int<lower = 0> K;       // number of covariates (including the intercept)
        int<lower = 0> nGr;     // number of groups

        vector[N] Y;            // response
        matrix[N, K] X;	        // covariates (fixed effects)
        matrix[N, nGr] G;       // groups allocation (regime effect)

	// prior parameters
        vector[K] mean_theta;	
        vector[K] var_theta;
        real<lower = 0> scale_s2;
}

parameters{
        real<lower = 0> sigma2;
        vector[nGr] sigma2_regime;

        vector[K] theta;    // regression coefficients
        vector[nGr] gamma;  // regime specific effect
}

transformed parameters{
        vector[N] mu;
        for(i in 1:N){
            mu[i] = row(X, i) * theta + row(G, i) * gamma;
        }
}

model{
        // Prior:
        sigma2 ~ inv_gamma(2., scale_s2);
        for(k in 1:K) {
            theta[k] ~ normal(mean_theta[k], pow(var_theta[k], 0.5));
        }

        for(j in 1:nGr){
            sigma2_regime[j] ~ inv_gamma(2,10);
            gamma[j] ~ normal(0.0, pow(sigma2_regime[j], 0.5));
        }

        // Likelihood:
        Y ~ normal(mu, pow(sigma2, 0.5));
}

generated quantities{
        vector[N] log_lik;
        for (j in 1:N) {
            log_lik[j] = normal_lpdf(Y[j] | mu[j], pow(sigma2, 0.5));
        }
}
