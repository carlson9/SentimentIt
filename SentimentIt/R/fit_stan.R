#' Fit STAN Model
#'
#' Fit STAN model
#'
#'
#' @param data a dataset
#' 
#' @return STAN output
#'
#' @author David Carlson
#'
#' @rdname fit_stan
#'
#' @export
fit_stan <- function(data){
  require(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  y <- data$result[seq(1, dim(data)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data$document_id_old <- data$document_id
  data$document_id <- as.numeric(as.factor(data$document_id))
  g <- data$document_id[seq(1, dim(data)[1], by=2)]
  h <- data$document_id[seq(1, dim(data)[1], by=2) + 1]
  j <- as.numeric(data$worker_id[seq(1, dim(data)[1], by=2)])
  #unique(data$worker_id[seq(1, dim(data)[1], by=2)])
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  
  ### need to recode ids ###
  hold.ids <- sort(unique(g))
  hold.ids.real <- g
  #length(as.factor(g))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  
  fit <- stan(file="~/SentimentIt/SentimentIt/model_code.stan", data=c("y", "g", "h", "N", "M", "P", "j"),
              chains=3, iter=2500, seed=1234)
  return(fit)
}