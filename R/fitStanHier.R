#' Fit STAN Hierarchical Model
#'
#' Fit STAN hierarchical model
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#' @param n.cores Number of cores to be used in stan fit (default is 3)
#'
#' @return fitStanHier
#'
#' @author David Carlson
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{createHITSTimed}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#'
#' @rdname fitStanHier
#'
#' @export
fitStanHier <- function(data, hierarchy_data, hierarchy_var,
                        chains=3, iter=2500, seed=1234, n.cores=3){

  if(!is.null(n.core)){
    rstan_options(auto_write = TRUE)
    options(mc.cores = n.cores)
  }

  if(is.vector(data)){
    data <- readInData(data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension is incorrect")
  }

  if(!(hierarchy_var %in% colnames(hierarchy_data))){
    stop("hierarchy_var should be a name of a column in hierarchy_data")
  }

  y <- data1$result[seq(1, dim(data1)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data1$document_id_old <- data1$document_id
  data1$document_id <- as.numeric(as.factor(data1$document_id))
  g <- data1$document_id[seq(1, dim(data1)[1], by=2)]
  h <- data1$document_id[seq(1, dim(data1)[1], by=2) + 1]
  j <- as.numeric(data1$worker_id[seq(1, dim(data1)[1], by=2)])
  k <- as.numeric(as.factor(as.character(hierarchy_data1[,hierarchy_var])))
  #unique(data1$worker_id[seq(1,dim(data1)[1],by=2)])
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(k))
  ### need to recode ids ###
  #hold.ids <- sort(unique(g))
  #hold.ids.real <- g
  #length(as.factor(g))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  k <- as.numeric(as.factor(k))
  #q <- as.numeric(as.factor(q))

  fit_hier <- stan(file="~/SentimentIt/model_code_hier.stan",
              data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=chains, iter=iter, seed=seed, control=list(max_treedepth=50))
  return(fit_hier)
}

