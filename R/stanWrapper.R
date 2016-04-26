#' Fit STAN Model and Detect Outlying Workers
#'
#' Fit STAN model or detect outlying workers
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy (defalt is NULL)
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy (defalt is NULL)
#' @param returnFit Return a fit object if TRUE (degfalt is FALSE)
#' @param plot If TRUE, create a histogram with a rug plot (defalt is FALSE)
#' @param file Save the histogram to path and file name specified (defalt is NULL)
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#' @param n.cores Number of cores to be used in stan fit (default is 3)
#'
#' @return A list containing
#'  \item{outlying_workers}{Outlying workers' IDs who have beta coefficients less than 1 and answered more than 100}
#'  \item{stan_fit}{STAN output}
#'
#' @author David Carlson
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{createHITSTimed}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#'
#' @rdname stanWrapper
#'
#' @export
stanWrapper <- function(data, hierarchy_data=NULL, hierarchy_var=NULL,
                        returnFit=FALSE, plot=FALSE, file=NULL,
                        chains=3, iter=2500, seed=1234, n.cores=3){

  if(is.vector(data)){
    data <- readInData(data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension mismatches")
  }

  # fit fit_stan or fit_stan_hier
  if(is.null(hierarchy_data)==FALSE & is.null(hierarchy_var)==FALSE){
    fit <- fit_stan_hier(data1, hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                         chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }
  else{
    fit <- fit_stan(data1, chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }

  outlying <- check_workers(fit, data1, plot=plot, file=file)

  if(!returnFit){
    return(list(outlying_workers=outlying, stan_fit=NULL))
  }
  else{
    return(list(outlying_workers=outlying, stan_fit=fit))
  }
}
