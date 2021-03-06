#' Fit a Stan model with results from SentimentIt
#'
#' Fit a random utility model using Hamiltonian MCMC in Stan with data retrieved from the SentimentIt platform.
#'
#' @param email The researcher's email used for SentimentIt registration. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param password The researcher's password used for SentimentIt. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param data A csv file or a vector of batch numbers.
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Defalt is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return fit Stan fit object
#'
#' @author David Carlson
#'
#' @examples
#'
#' \dontrun{
#' fit <- fitStan(data = batch_ids)
#' fit <- fitStan(data = output)
#' }
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname fitStan
#' @export
fitStan <- function(email=NULL, password=NULL, data, chains=3, iter=2500, seed=1234, n.cores=3){
  requireNamespace('rstan') #bug in rstan - needs explicit call
  rstan_options(auto_write = TRUE)
  options(mc.cores = n.cores)

  if(is.vector(data)){
    data <- readInData(email, password, data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension mismatches")
  }

  y <- data1$result[seq(1, dim(data1)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data1$document_id_old <- data1$document_id
  data1$document_id <- as.numeric(as.factor(data1$document_id))
  g <- data1$document_id[seq(1, dim(data1)[1], by=2)]
  h <- data1$document_id[seq(1, dim(data1)[1], by=2) + 1]
  j <- as.numeric(as.factor(data1$worker_id[seq(1, dim(data1)[1], by=2)]))
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  model_code <- '
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
real<lower=0> sigma;  
}
model {
sigma~normal(0,3);
for(p in 1:P){
b[p] ~ normal(0,sigma);
}
for(m in 1:M){
a[m] ~ normal(0,1);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'

  fit <- stan(model_code = model_code, data=c("y", "g", "h", "N", "M", "P", "j"),
              chains=chains, iter=iter, seed=seed)
  return(fit)
}
