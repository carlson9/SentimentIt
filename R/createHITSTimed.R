#' Create HITS Timed
#'
#' Creates batches, and waits a certain 
#'
#' @param batches Vector of Batch numbers to check
#'
#' @return
#' @author Jacob M. Montgomery
#' @note 
#' @examples
#'
#' @rdname createHITSTimed
#' @export
createHITStimed <- function(batches, time_per, mintime, 
                            maxtime, worker_check=NULL){
  out <- vector()
  for(i in batches){
    checkTime(mintime, maxtime)
    x <- createHITS(batch_id=i)
    out[i] <- x
    Sys.sleep(time_per*3600)
    # TODO: Add in fitStan and checkWorkers to check and ban workers
    # every time an indicated batch is run.
    if(any(i %in% worker_check)) {
      stanWrapper(data=i)
    }
    # fitSTan, checkWorkers
    #could alter above to check if batch is completed
  }
  return(out)
}
checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}