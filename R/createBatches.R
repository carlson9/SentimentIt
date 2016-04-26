#' Create new batches
#' 
#' This function creates new batches with the desired Task setting 
#' and the desired number of batches.
#' 
#' @param hit_setting_id ID of Task setting to use
#' @param num_batches number of separate batches to create
#'
#' @return batch_ids ID of batches created
#' @author David Carlson
#' @examples
#' \dontrun{
#' createBatches(hit_setting_id=2, num_batches=4)
#' createBatches(hit_setting_id=2)
#' }
#' @rdname createBatches
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createTasksTimed}},
#' \code{\link{createCert}},\code{\link{createTasks}}, \code{\link{createTasksBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
createBatches <- function(hit_setting_id, num_batches=1){
  if(!is.numeric(hit_setting_id)){
    stop("hit_setting_id needs to be a numeric")
  }
  batch_ids <- vector()
  for(i in 1:num_batches){
    args <- paste("hit_setting_id=", hit_setting_id, sep="")
    myget <- POST(paste('http://sentimentit.herokuapp.com/api/batches.json?',
                       args, sep=''))
    batch_ids[i] <- fromJSON(rawToChar(as.raw(myget$content)))$id
  }
  return(batch_ids)
}
