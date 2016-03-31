#####
readInData <- function(batchNumber) {
  # Required packages 
  require(httr)
  require(jsonlite)
  require(RCurl)
  length_batch <- length(batchNumber)

  # try_count will count how many times function tries to connect to server
  # Will try five times before giving up
  try_count <- 0
  #https://sentimentit.com/api/batches/1/download.json 
  while(try_count <= 5){
    # TODO: Currently code resets output matrix each try. I am unsure if it should
    # keep batch numbers which work successfully.
    output_matrix <- NULL
    for (i in 1:length_batch){
      output<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',batchNumber[i],'/download.json'))
      myurl <- rawToChar(as.raw(output$content))
      myurl <- strsplit(myurl,'\"')[[1]][4]
      x <- getURL(myurl)
      data <- read.csv(text = x)
      output_matrix <- cbind(output_matrix, data)
    }
    # check if output is correct. If not function tries again.
    if(nrow(output_matrix) > 1 & ncol(output_matrix) == length_batch ) {
      try_count <- 6
    } else {
      print("Can not connect to server. Will try again in two minutes.")
      try_count <- try_count + 1
      Sys.sleep(120)
    }
  }
  if(nrow(output_matrix) < 1 | ncol(output_matrix) != length_batch){
    return("Could not connect to server now. Please check your batch numbers are accurate and try again later.")
  }
  batch_names <- paste("batch ", batchNumber)
  colnames(output_matrix) <- batch_names
  return(output_matrix)
}



