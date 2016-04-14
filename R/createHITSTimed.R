#' Wrapper Timed
#'
#' Creates batches, and waits a certain 
#'
#' @param batches Vector of Batch numbers to check
#'
#' @return out ID for batch of comparisons
#' @author Jacob M. Montgomery
#' @note 
#' @examples
#'
#' @rdname createHITSTimed
#' @export
createHITStimed <- function(batches, time_per, mintime, maxtime, checkWorkersAt=NULL){
  out <- vector()
  for(i in batches){
    checkTime(mintime, maxtime)
    x <- createHITS(batch_id=i)
    out[i] <- x
    Sys.sleep(time_per*3600)
    if(any(i %in% checkWorkersAt)) {
      stanWrapper(data=i)
    }
  }
  return(out)
}
checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}