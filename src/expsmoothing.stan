data {
	int N;
	vector[N] y;
	int p;
}
parameters {
	real<lower = 0,upper = 1> alpha;
	real<lower = 0,upper = 1> beta;
	real<lower = 0,upper = 1> gamma;
	real<lower = 0> sigma;
	real b0;
	real a0;
	vector[p] c0;
}
transformed parameters {
	vector[N] a; 	
	vector[N] b;
	vector[N] c;
	a[1] = alpha*(y[1]-c0[1]) + (1- alpha)*(a0 + b0);
	b[1] = beta*(a[1] - a0) + (1-beta)*b0;
	c[1] = gamma*(y[1] - a0 - b0) + (1-gamma)*c0[1];
	for(t in 2:p) {
		a[t] = alpha*(y[t]-c0[t]) + (1 - alpha)*(a[t-1] + b[t-1]);
		b[t] = beta*(a[t] - a[t-1]) + (1-beta)*b[t-1];
		c[t] = gamma*(y[t] - a[t-1] - b[t-1]) + (1-gamma)*c0[t];
	}
	for(t in (p+1):N) {
		a[t] = alpha*(y[t]-c[t-p]) + (1 - alpha)*(a[t-1] + b[t-1]);
		b[t] = beta*(a[t] - a[t-1]) + (1-beta)*b[t-1];
		c[t] = gamma*(y[t] - a[t-1] - b[t-1]) + (1-gamma)*c[t-p];
	}
}
model {
	y[1] ~ normal(a0 + b0 + c0[1], sigma);

	for(t in 2:p) {
		y[t] ~ normal(a[t-1]+ b[t-1] + c0[t], sigma);
	}

	for(t in (p+1):N) {
		y[t] ~ normal(a[t-1]+ b[t-1] + c[t-p], sigma);
	}

	y ~ normal(a + b, sigma);
	target += log(1/sigma);
}
