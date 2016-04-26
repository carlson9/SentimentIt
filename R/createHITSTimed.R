#' Create HITS Timed
#'
#' Creates batches, and waits a certain
#'
#' @param batches Vector of Batch numbers to check
#' @param certone The name of the certification wanted for the workers.
#' @param certtwo The name of the certification wanted to be removed from the workers.
#' @param time_per Time per batch to post on to Mechanical Turk
#' @param mintime The earliest time in the morning to post comparisons to workers
#' @param maxtime The latest time at night to post comparisons to workers
#' @param certone Certification to give
#' @param certtwo Certification to revoke
#' @param checkWorkersAt batch positions to check workers(i.e. batches 1, 3, 5)
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
#' @return out IDs for batches of comparisons
#' @author Jacob M. Montgomery
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#' @rdname createHITSTimed
#' @export
createHITStimed <- function(batches, time_per, mintime,
                            maxtime, certone, certtwo, checkWorkersAt=NULL,hierarchy_data=NULL,
                            hierarchy_var=NULL, returnFit=FALSE, plot=FALSE, file=NULL,
                            chains=3, iter=2500, seed=1234, n.cores=3){
  out <- vector()
  banned_workers <- vector()
  for(i in batches){
    checkTime(mintime, maxtime)
    x <- createHITS(batch_id=i)
    out <- c(out, x)
    Sys.sleep(time_per*3600)
    if(i %in% batches[checkWorkersAt]) {
       givetakeCert(certone, certtwo, stanWrapper(data=batches[1:length(out)],hierarchy_data=hierarchy_data,
                    hierarchy_var=hierarchy_var, returnFit=returnFit, plot=plot, file=false,
                    chains=chains, iter=iter, seed=seed, n.cores=n.cores)[[1]])
    }
  }
  return(out)
}
checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}
