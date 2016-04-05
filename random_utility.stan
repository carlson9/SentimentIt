data {
  int N; // number of comparisons
  int M; // number of documents
  int y[N]; // outcome
  int g[N];    // id  map first item in comparison
  int h[N];    // id map of second itein comparison
}
parameters {
  real a[M]; 
  real<lower=0> sigma;  
}
model {
  sigma~normal(0,3);
  for(m in 1:M){
    a[m] ~ normal(0,sigma);
  }
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit(a[g[n]]-a[h[n]]));
  }
}