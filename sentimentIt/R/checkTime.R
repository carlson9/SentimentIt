#' @export
.checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+min_time)*3600)
  if(current<=mintime) Sys.sleep((min_time-current)*3600)
}

