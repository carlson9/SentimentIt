#' Check batch status
#' 
#' This function checks the indicated batch_id numbers and returns their status. This returns a data frame with the batch ID and the number of comparisons submitted and the number completed.
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param batch_id Vector of batch IDs to check
#'
#' @return output Dataframe with the variables:
#' \itemize{
#'   \item id
#'   \item total_count
#'   \item submitted count
#'   \item completed_count
#'   \item expired_count
#' }
#' @author David Carlson
#' @examples
#' 
#' \dontrun{ 
#' batch_id <- 204:208
#' status <- batchStatus(email, password, batch_id)
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname batchStatus
#' @export
batchStatus <- function(email, password, batch_id){
  auth_token <- authenticate(email, password)
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  output<-data.frame()
  for(i in batch_id){
    myurl<- GET(paste0('http://sentimentit.com/api/batches/',i,'/status.json?email=',email,'&auth_token=', auth_token))
    if(rawToChar(as.raw(myurl$content))!='  '){
      data <- fromJSON(rawToChar(as.raw(myurl$content)))
      status <- data.frame(data$id, data$comparisons)
      output <- rbind(output, status)
    }else{
      print(paste('Check batch',i,'-- nonexistent.'))
    }
  }
  if(dim(output)[1]==0){
    print('Check batch number(s). Batch(es) non-existent.')
    output <- data.frame(matrix(0, nrow=1,ncol=5)) 
  }
  colnames(output) <- c("id", "total_count", "submitted_count", "completed_count", "expired_count" )
  return(output)
}

