#' Fit STAN Model
#'
#' Fit STAN model
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#'
#' @return STAN output
#'
#' @author David Carlson
#'
#' @seealso \code{\link{fit_stan_hier}}, \code{\link{ckeck_workers}}, \code{\link{stanWrapper}}
#'
#' @rdname fit_stan
#'
#' @export
fit_stan <- function(data, chains=3, iter=2500, seed=1234){

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  if(is.vector(data)==TRUE){
    data <- readInData(data)
  }

  if(dim(data)[2] != 7){
    stop("data dimension mismatches")
  }

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
  #hold.ids <- sort(unique(g))
  #hold.ids.real <- g
  #length(as.factor(g))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))

  fit <- stan(file="~/SentimentIt/model_code.stan", data=c("y", "g", "h", "N", "M", "P", "j"),
              chains=chains, iter=iter, seed=seed)
  return(fit)
}
