#' Fit STAN Model and Detect Outlying Workers
#'
#' A wrapper function for fitStan, fitStanHier, and checkWorkers. Fit STAN model and/or detect outlying workers.
#'
#'
#' @param data A csv file or a vector of batch numbers.
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy. (Default is NULL)
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy. (Default is NULL)
#' @param returnFit Return a fit object if TRUE. (Default is FALSE)
#' @param cut_point A cutoff point to classify posterior coefficients. The proportion of posterior coefficients below \code{cut_point} is used to determine outliers. (Default is 1)
#' @param cut_proportion A cutoff proportion of posterior coefficients below \code{cut_point}. If the proportion of posterior coefficients below \code{cut_points} is higher than \code{cut_proportion}, a worker will be considered as an outlier provided that she answers more than 50 questions. (Default is 0.9)
#' @param plot_hist If TRUE, plot the histogram of workers with a rug plot. (Default is FALSE)
#' @param file_path Save the histogram to path and file name specified. (Default is NULL)
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Default is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return A list containing
#'  \item{outlying_workers}{A vector of outlying workers' IDs whose proportion of posterior coefficients below \code{cut_point} is greater than \code{cut_proportion} and who answered more than 50 questions.}
#'  \item{stan_fit}{STAN output (class stanfit)}
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
.stanWrapper <- function(data, hierarchy_data=NULL, hierarchy_var=NULL,
                        returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                        plot_hist=FALSE, file_path=NULL,
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
    fit <- fitStanHier(data=data1, hierarchy_data=hierarchy_data,
                       hierarchy_var=hierarchy_var,
                       chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }
  else{
    fit <- fitStan(data=data1, chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }

  outlying <- checkWorkers(stan_fit=fit, data=data1, cut_point=cut_point,
                           cut_proportion=cut_proportion,
                           plot_hist=plot_hist, file_path=file_path)

  if(!returnFit){
    return(list(outlying_workers=outlying, stan_fit=NULL))
  }
  else{
    return(list(outlying_workers=outlying, stan_fit=fit))
  }
}
