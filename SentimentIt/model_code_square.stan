data {
  int N; // number of comparisons
  int M; // number of documents
  int P; //Number of coders
  int y[N]; // outcome
  int g[N];    // id  map first item in comparison
  int h[N];    // id map of second itein comparison
  int j[N]; // id map for workers
}
parameters {
  real a[M];
  real<lower=0> b[P];
  //real<lower=0> sigma;  
}
model {
  //sigma~normal(0,3);
  for(p in 1:P){
    b[p] ~ normal(0,1);
  }
  for(m in 1:M){
    a[m] ~ normal(0,1);
  }
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit(b[j[n]]*pow(a[g[n]]-a[h[n]],2)*fabs(a[g[n]]-a[h[n]])/(a[g[n]]-a[h[n]])));
  }
}