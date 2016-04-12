#' Create New Hits for comparisons
#' 
#' This function takes in id numbers of texts and hit settings, or a batch id, and it creates 
#' HITS for comparison.
#' 
#' @param ids Comma-separated list of comparison ids for which to create hits
#' @param HITsetting id of the HIT Setting to use for the created HITs
#' @param batch_id id of the Batch of comparisons
#'
#' @return out ID for batch of comparisons
#' @author David Carlson
#' @note This function requires the usage of the httr and jsonlite packages.
#' @examples
#'
#' createHITS(ids=10,HITsetting=2)
#' createHITS(batch_id=204)
#' @rdname createHITS
#' @export
createHITS <- function(ids=NULL, HITsetting=NULL, batch_id){
  require(httr)
  require(jsonlite)
  if(!is.numeric(ids) | !is.numeric(HITsetting) | !is.numeric(batch_id)){
    stop("All arguments need to be numeric.")
  }
  if(is.null(ids)){
    args <- paste('batch_id=', batch_id,"" ,sep='')
  }else{
    args <- paste('hit_setting=', HITsetting, '&ids=', paste(ids,collapse=','), sep='')
  }
  myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                     args, sep=''))
  mytry <- try(out<-fromJSON(rawToChar(as.raw(myget$content))))
  if(class(mytry) == "try-error"){
    out <- 'error'
  }
  return(out)
}

