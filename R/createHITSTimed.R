#' Create HITS Timed
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