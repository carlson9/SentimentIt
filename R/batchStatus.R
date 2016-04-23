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
#' @seealso \code{\link{batchesWrapper}}, \code{\link{sentimentIt}}
#' @examples
#' 
#' \dontrun{ 
#' x <- 204:208
#' status <- batchStatus(x)
#' y <- c(204, 206, 208, 207)
#' status2 <- batchStatus(y)
<<<<<<< Updated upstream
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
=======
#' }
>>>>>>> Stashed changes
#' @rdname batchStatus
#' @export
batch_status <- function(batch_id){
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  if(is.negative(batch_id))
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



