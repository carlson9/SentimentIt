#' Fit STAN Model
#'
#' Fit STAN model
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#' @param n.core Number of cores to be used in stan fit (default is 3)
#'
#' @return STAN output
#'
#' @author David Carlson
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{createHITSTimed}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#'
#' @rdname fitStan
#'
#' @export
fitStan <- function(data, chains=3, iter=2500, seed=1234, n.core=3){

  if(!is.null(n.core)){
    rstan_options(auto_write = TRUE)
    options(mc.cores = n.core)
  }

  if(is.vector(data)){
    data <- readInData(data)
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
  j <- as.numeric(data1$worker_id[seq(1, dim(data1)[1], by=2)])
  #unique(data1$worker_id[seq(1, dim(data1)[1], by=2)])
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
