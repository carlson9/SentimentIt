#' Create HITS timed by batch progress.
#'
#' Creates batches, and waits a certain time based on the progress of batch.
#'
#' @param batches Vector of Batch numbers to check.
#' @param min_time Earliest that hits can be created (default = 9:00).
#' @param max_time Latest time that hits can be created (default = 22:00).
#' @param threshold Point at which a batch is considered done (default = 5).
#' @param rate The rate by which the progress of a batch will be checked (default = 1/3 an hour).
#' 
#' 
#' @return out ID for batch of comparisons
#' @author Jacob M. Montgomery
#' @note 
#' @examples
#'
#' @rdname createHITSBatch
#' @export
createHITSBatch <- function(batches, min_time=9, 
                   max_time=22, rate=1/3, threshold=5, checkWorkersAt=NULL){
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
      givetakeCert(certone, certtwo, stanWrapper(data=batches[1:length(out)])[[1]])    
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
