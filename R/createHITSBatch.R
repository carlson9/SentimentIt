#' Create HITS timed by batch progress.
#'
#' Creates batches, and waits a certain time based on the progress of batch.
#'
#' @param batches Vector of Batch numbers to check.
#' @param min_time Earliest that hits can be created.
#' @param max_time Latest time that hits can be created.
#' @param wait_time Time to wait till next hit creation.
#' @param threshold Point at which a batch is considered done.
#' @param rate The rate by which the progress of a batch will be checked.
#' 
#' 
#' @return out ID for batch of comparisons
#' @author Jacob M. Montgomery
#' @note 
#' @examples
#'
#' @rdname createHITSBatch
#' @export
createHITSBatch <- function(batches, min_time, 
                   max_time, time_per, threshold, checkWorkersAt=NULL){
  out <- vector()
  for(i in batches){
    checkTime(min_time, max_time)
    x <- createHITS(batch_id=i)
    out[i] <- x
    done <- FALSE
    Sys.sleep(time_per*3600)
    for(i in batches){
      checkTime(min_time, max_time)
      x <- createHITS(batch_id=i)
      out[i] <- x
      done <- FALSE
      while(!done){
        Sys.sleep(rate*60)
        status <- batchStatus(batches[i])
        done <- ((status$submitted_count - status$completed_count) <= threshold)
      }
    if(i %in% batches[checkWorkersAt]) {
      givetakeCert(certone, certtwo, stanWrapper(data=i)[[1]])    
    }
  }
  return(out)
}
checkTime <- function(min_time, max_time){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=max_time) Sys.sleep((24-current+min_time)*3600)
  if(current<=min_time) Sys.sleep((min_time-current)*3600)
}
}