#' Create HITS Timed
#'
#' Creates batches, and waits a certain 
#'
#' @param batches Vector of Batch numbers to check
#' @param time_per Time per batch to post on to Mechanical Turk
#' @param mintime The earliest time in the morning to post comparisons to workers
#' @param maxtime The latest time at night to post comparisons to workers
#' @param certone Certification to give
#' @param certtwo Certification to revoke
#' @param checkWorkersAt batch positions to check workers(i.e. batches 1, 3, 5)
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
                            maxtime, certone, certtwo, checkWorkersAt=NULL){
  out <- vector()
  banned_workers <- vector()
  for(i in batches){
    checkTime(mintime, maxtime)
    x <- createHITS(batch_id=i)
    out <- rbind(out, x)
    Sys.sleep(time_per*3600)
    if(i %in% batches[checkWorkersAt]) {
       givetakeCert(certone, certtwo, stanWrapper(data=i)[[1]])
    }
  }
  return(out)
}
checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}
