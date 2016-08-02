#' Read in Data
#'
#' Reads in data for specified batch numbers from the server
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt
#' @param batch_id A vector of batch numbers to download data from.
#'
#' @return output a dataframe with the data from the specified batches with columns
#' \describe{
#'   \item{batch_id}{the batch number}
#'   \item{comparison_id}{id number of the comparison being made}
#'   \item{document_id}{id number of the document}
#'   \item{result}{result of document comparison}
#'   \item{task_id}{id of the task}
#'   \item{worker_id}{id of the worker who did the comparison}
#'   \item{completed_at}{time comaparison was completed}
#' }
#' @author Jacob M. Montgomery
#' @examples
#' 
#' \dontrun{
#' x <- 204:208
#' myData <- readInData(email, password, x)
#' }
#' @rdname readInData
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
readInData <- function(email, password, batch_id) {
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be numeric")
  }
  auth_token <- authenticate(email, password)
  # Put the batch_id in numerical order and remove duplicates
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  output<-data.frame()
  for(i in batch_id){
    myurl<- GET(paste0('https://www.sentimentit.com/api/batches/',i,'/download.json?email=', email, '&auth_token=', auth_token))
    myurl <- rawToChar(as.raw(myurl$content))
    myurl <- strsplit(myurl,'\"')[[1]][4]
    x <- getURL(myurl)
    # attempt to connect to server until data is downloaded
    try_count <- 0
    while(nchar(x) < 100 & try_count < 15){
      Sys.sleep(20)
      x <- getURL(myurl)
      try_count = try_count + 1
    }
    if(nchar(x) < 100 & try_count == 15){
      message <- paste("Failed to download batch", i,
                       "try again later and check that the batch number exists.")
      warning(message)
    }else{
      hold <- as.data.frame(read.csv(text = x), stringsAsFactors = FALSE)
      for(k in 1:ncol(hold)){
        if(is.factor(hold[,k])) hold[,k] <- as.character(hold[,k])
      }
      output <- rbind(output,hold)
    }
  }
  return(output)
}

