data {
int N; // number of comparisons
int M; // number of paragraphs
int D; // number of documents (countries)
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second item in comparison
int j[N]; // id map for workers
int k[N]; // id map for documents (countries) of first item
int q[N]; // id map for documents of second item
}
parameters {
real a[M]; // paragraphs
real t[D]; // documents (countries)
real<lower=0> b[P];
real<lower=0> sigma;  
real<lower=0> sigmac[D];
}
model {
sigma~normal(0,3);
t ~ normal(0, 1);
//for(p in 1:P){
b ~ normal(0,sigma);
//}
//for(d in 1:D){
t ~ normal(0,1);
//}
//for(m in 1:M){
//a[m] ~ normal(0,1);
//}
sigmac ~ normal(0,3);
for(n in 1:N) {
a[g[n]] ~ normal(t[k[n]], sigmac[k[n]]);
a[h[n]] ~ normal(t[q[n]], sigmac[q[n]]);
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}
