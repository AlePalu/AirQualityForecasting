data {
	int N;
	vector[N] y;
	int p;
}
transformed data{
	matrix[N,p+1] M;
	int m;
	int R;
	for(i in 1:N) {
		M[i,1] = i-1;
	}

	m = N/p;
	R = N % p;
	for(k in 1:m) {
  		for(i in 1:p) {
    			for(j in 1:p) {
      				if(i == j){
        				M[i + p*(k-1),j+1] = 1;
      				}
      				else {
      	  				M[i + p*(k-1),j+1] = 0;
      				}
    			}
  		}
	}
	for(i in 1:R) {
  		for(j in 1:p) {
    			if(i == j){
      				M[i + p*(m),j+1] = 1;
    			}
    			else {
      				M[i + p*(m),j+1] = 0;
    			} 
  		}
	}

}
parameters {
	real<lower = 0,upper = 1> alpha;
	real<lower = 0,upper = 1> beta;
	real<lower = 0,upper = 1> gamma;
	real<lower = 0> sigma;
	real b0;
	vector[p] c0;
}
transformed parameters {
	vector[p+1] psi;
	matrix[N,N] L;
	psi[1] = b0;
	psi[2:(p+1)] = c0[1:p];
	{
	int ll;
	for(i in 1:N) {
  		for(j in 1:N) {
			L[i,j] = alpha * (1 + (i-j)*beta);	
			ll = i-j+1;
			if(ll % p == 1) {
				L[i,j] = alpha * (1 + (i-j)*beta) + gamma;	
			}
			if(i==j){
				L[i,j] = 1;
			}
			if(j>i) {
				L[i,j] = 0;
			}
    		}
	}
	}

}
model {
	y ~ multi_normal(M*psi, (L*L')*sigma*sigma);
	target += log(1/sigma);
}
