#' Fit STAN Hierarchical Model
#'
#' Fit STAN hierarchical model
#'
#'
#' @param data a dataset
#'
#' @return STAN output
#'
#' @author David Carlson
#'
#' @rdname fit_stan_squre
#'
#' @export
fit_stan_square <- function(data){
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  y <- data$result[seq(1,dim(data)[1],by=2)]
  #z <- y
  #z[z==0] <- -1
  data$document_id_old <- data$document_id
  data$document_id <- as.numeric(as.factor(data$document_id))
  #data$country_id_old <- data$countries
  #data$country_id <- as.numeric(data$country_id_old)
  g <- data$document_id[seq(1,dim(data)[1],by=2)]
  h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
  countries <- as.numeric(as.factor(data$countries))
  j <- as.numeric(data$worker_id[seq(1,dim(data)[1],by=2)])
  #unique(data$worker_id[seq(1,dim(data)[1],by=2)])
  M <- length(unique(c(g,h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(c(q,k)))
  ### need to recode ids ###
  #hold.ids <- sort(unique(g))
  #hold.ids.real <- g
  #length(as.factor(g))
  #g <- as.numeric(as.factor(g))
  #h <- as.numeric(as.factor(h))
  #k <- as.numeric(as.factor(k))
  #q <- as.numeric(as.factor(q))

  fit <- stan(file="~/SentimentIt/SentimentIt/model_code_hier.stan", data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=3, iter=2500, seed=1234, control=list(max_treedepth=50))
  return(fit)
}
