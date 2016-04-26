#' Create HITS timed by batch progress.
#'
#' Creates batches, and waits a certain time based on the progress of batch.
#'
#' @param batches Vector of Batch numbers to check.
#' @param certone The name of the certification wanted for the workers.
#' @param certtwo The name of the certification wanted to be removed from the workers.
#' @param min_time Earliest that hits can be created (default = 9:00).
#' @param max_time Latest time that hits can be created (default = 22:00).
#' @param threshold Point at which a batch is considered done (default = 5).
#' @param rate The rate by which the progress of a batch will be checked (default = 1/3 an hour).
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
#' @return out ID for batch of comparisons
#' @author Jacob M. Montgomery
#' @note
#' @examples
#'
#' @rdname createHITSBatch
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSTimed}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
createHITSBatch <- function(batches, certone, certtwo, min_time=9,
                   max_time=22, rate=1/3, threshold=5, checkWorkersAt=NULL,
                   hierarchy_data=NULL, hierarchy_var=NULL,
                   returnFit=FALSE, plot=FALSE, file=NULL,
                   chains=3, iter=2500, seed=1234, n.cores=3){
  if(!is.character(certone) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(certtwo) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(certone == certtwo){
    stop("The certifications you are giving and taking away cannot be made the same.")
  }
  if(!is.numeric(batches) | nchar(batches)<1){
    stop("The batch numbers must be numerical digits and non-blank.")
  }
  out <- vector()
  for(i in batches){
    checkTime(min_time, max_time)
    x <- createHITS(batch_id=i)
    out[i] <- x
    done <- FALSE
    current <- as.numeric(format(Sys.time(), "%M"))
      while(!done){
        Sys.sleep(rate*3600)
        status <- batchStatus(batches[i])
        done <- (((status$submitted_count - status$completed_count) <= threshold) | (as.numeric(format(Sys.time(), "M%")) - current > 240))
      }
    if(i %in% batches[checkWorkersAt]) {
      givetakeCert(certone, certtwo, stanWrapper(data=batches[1:length(out)],
                   hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                   returnFit=returnFit, plot=plot, file=file,
                   chains=chains, iter=iter, seed=seed, n.cores=n.cores)[[1]])
    }
  }
  return(out)
}
#' @export
checkTime <- function(min_time, max_time){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=max_time) Sys.sleep((24-current+min_time)*3600)
  if(current<=min_time) Sys.sleep((min_time-current)*3600)
}
}
