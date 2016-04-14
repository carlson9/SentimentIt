#' Check batch status
#' 
#' This function checks the indicated batch_id numbers and returns their status.
#' 
#' @param batch_id ID of batch to check
#'
#' @return output dataframe with the variables:
#' \itemize{
#'   \item id
#'   \item total_count
#'   \item submitted count
#'   \item completed_count
#'   \item expired_count
#' }
#' @author David Carlson
#' @note This function requires the usage of the jsonlite packages
#' @examples
#'
#' x <- 204:208
#' status <- batchStatus(x)
#' y <- c(204, 206, 208, 207)
#' status2 <- batchStatus(y)
#' @rdname batchStatus
#' @export
batch_status <- function(batch_id){
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  output<-data.frame()
  for(i in batch_id){
    myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',i,'/status.json'))
    data <- fromJSON(rawToChar(as.raw(myurl$content)))
    status <- data.frame(data$id, data$comparisons)
    output <- rbind(output, status)
  }
  colnames(output) <- c("id", "total_count", "submitted_count", "completed_count", "expired_count" )
  return(output)
}

