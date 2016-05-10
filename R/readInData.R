#' Read in Data
#'
#' Reads in data for specified batch numbers from the website
#'
#' @param batchNumbers a vector of batch numbers to download data for.
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
#' @note Sometimes the server will fail to send the data to your computer,
#'      so this function will try again 10 times until it successfully connects to the server.
#' @examples
#' 
#' \dontrun{
#' x <- 204:208
#' myData <- readInData(x)
#' y <- c(204, 206, 208, 207)
#' myData2 <- readInData(y)
#' }
#' @rdname readInData
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}
#' }
#' @export
readInData <- function(batchNumbers) {
  if (!is.vector(batchNumbers) | !is.numeric(batchNumbers)) {
    stop("batchNumbers needs to be a vector of numerics")
  }
  # Put the batchNumbers in numerical order and remove duplicates
  batchNumbers <- unique(batchNumbers)
  batchNumbers <- batchNumbers[sort.list(batchNumbers)]
  output<-data.frame()
  for(i in batchNumbers){
    myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',i,'/download.json'))
    myurl <- rawToChar(as.raw(myurl$content))
    myurl <- strsplit(myurl,'\"')[[1]][4]
    x <- getURL(myurl)
    # attempt to connect to server until data is downloaded
    try_count <- 0
    while(nchar(x) < 1000 & try_count < 10){
      Sys.sleep(20)
      x <- getURL(myurl)
      try_count = try_count + 1
    }
    if(nchar(x) < 1000 & try_count == 10){
      message <- paste("Failed to download batch", i,
                       "try again later and check that the batch number exists.")
      warning(message)
    }
    myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',i,'/download.json'))
    hold <- as.data.frame(read.csv(text = x), stringsAsFactors = FALSE)
    for(k in 1:ncol(hold)){
      if(is.factor(hold[,k])) hold[,k] <- as.character(hold[,k])
    }
    output <- rbind(output,hold)
  }
 return(output)
}




