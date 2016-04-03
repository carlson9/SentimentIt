#' Read in Data
#'
#' Reads in data for specified batch numbers from the website
#'
#' @param batchNumbers a vector of batchnumbers 
#'
#' @return output a dataframe with the data from the specified rows
#' @author Jacob M. Montgomery
#' @note Sometimes the server will fail to send the data to your computer, 
#'      so this function will try again until it successfully connects to the server.
#' @examples
#' x <- 204:208
#' myData <- readInData(x) 
#' y <- c(204, 206, 208, 207)
#' myData2 <- readInData(y)
#' @seealso \code{\link{subtractSquares}}, \code{\link{addSquares}}
#' @rdname readInData
readInDataHelper <- function(batchNumber) {
  myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',batchNumber,'/download.json'))
  myurl <- rawToChar(as.raw(myurl$content))
  myurl <- strsplit(myurl,'\"')[[1]][4]
  x <- getURL(myurl)
  while(nchar(x)<1000){
    Sys.sleep(20)
    x <- getURL(myurl)
  }
  myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',batchNumber,'/download.json'))
  hold <- as.data.frame(read.csv(text = x), stringsAsFactors = FALSE)
  for(k in 1:ncol(hold)){
    if(is.factor(hold[,k])) hold[,k] <- as.character(hold[,k])
  }
}
#' @export
readInData <- function(batchNumbers) {
  # Required packages 
  require(httr)
  require(jsonlite)
  require(RCurl)
  require(plyr)
  # I use adply because it is easier to do non-sequential 
  # batch numbers with apply than with a for loop and it allows me to
  # control inputs into the function.
  output <- adply(.data=batchNumbers, 
                  .fun=readInDataHelper, .margins=1)
  #https://sentimentit.com/api/batches/1/download.json 
  batch_names <- paste("batch ", batchNumbers)
  colnames(output) <- batch_names
  return(output)
}




