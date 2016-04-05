#' Read in Data
#'
#' Reads in data for specified batch numbers from the website
#'
#' @param batchNumbers a vector of batch numbers to download data for.  
#'
#' @return output a dataframe with the data from the specified batches
#' @author Jacob M. Montgomery
#' @note Sometimes the server will fail to send the data to your computer, 
#'      so this function will try again until it successfully connects to the server.
#' @examples
#' 
#' x <- 204:208
#' myData <- readInData(x) 
#' y <- c(204, 206, 208, 207)
#' myData2 <- readInData(y)
#' @rdname readInData
#' @export
readInData <- function(batchNumbers) {
  # Required packages 
  require(httr)
  require(jsonlite)
  require(RCurl)
  batchNumbers <- batchNumbers[sort.list(batchNumbers)]
  output<-data.frame()
  for(i in batchNumbers){
    myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',i,'/download.json'))
    myurl <- rawToChar(as.raw(myurl$content))
    myurl <- strsplit(myurl,'\"')[[1]][4]
    
    x <- getURL(myurl)
    while(nchar(x)<1000){
      Sys.sleep(20)
      x <- getURL(myurl)
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






