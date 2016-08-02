#' Fit Stan Hierarchical Model
#'
#' Fit a Stan heirarchical model with data retrieved from the SentimentIt platform.
#'
#' @param email The researcher's email used for SentimentIt registration. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param password The researcher's password used for SentimentIt. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param data A csv file or a vector of batch numbers.
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy.
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy.
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Default is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return fit_heir The heirarchical Stan fit object
#'
#' @author David Carlson
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname fitStanHier
#' @export
fitStanHier <- function(email=NULL, password=NULL, data, hierarchy_data, hierarchy_var,
                        chains=3, iter=2500, seed=1234, n.cores=3){

  rstan_options(auto_write = TRUE)
    options(mc.cores = n.cores)

  if(is.vector(data)){
    data <- readInData(email, password, data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension is incorrect")
  }

  if(!(hierarchy_var %in% colnames(hierarchy_data))){
    stop("hierarchy_var should be a name of a column in hierarchy_data")
  }

  y <- data1$result[seq(1, dim(data1)[1], by=2)]
  data1$document_id_old <- data1$document_id
  data1$document_id <- as.numeric(as.factor(data1$document_id))
  g <- data1$document_id[seq(1, dim(data1)[1], by=2)]
  h <- data1$document_id[seq(1, dim(data1)[1], by=2) + 1]
  j <- as.numeric(data1$worker_id[seq(1, dim(data1)[1], by=2)])
  k <- as.numeric(as.factor(as.character(hierarchy_data[,hierarchy_var])))
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(k))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  k <- as.numeric(as.factor(k))
  model_code <- '
data {
int N; // number of comparisons
int M; // number of paragraphs
int D; // number of documents (countries)
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second item in comparison
int j[N]; // id map for workers
int k[M]; // id map for documents (countries) relating to documents
}
parameters {
real a[M]; // paragraphs
real t[D]; // documents (countries)
real<lower=0> b[P];
real<lower=0> sigmac[D];
}
model {
for(p in 1:P){
b[p] ~ normal(0,1);
}
for(d in 1:D){
t[d] ~ normal(0,1);
sigmac[d] ~ normal(0,.5);
}
for(m in 1:M){
a[m] ~ normal(t[k[m]],sigmac[k[m]]);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'

  fit_hier <- stan(model_code = model_code,
              data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=chains, iter=iter, seed=seed, control=list(max_treedepth=50))
  return(fit_hier)
}

