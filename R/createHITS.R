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
#'
#' @author David Carlson
#'
#' @note This function requires the usage of the httr and jsonlite packages.
#' @rdname createHITS
#' @export
create_hits<-function(ids=NULL, HITsetting=NULL, batch_id){
  require(httr)
  require(jsonlite)
  if(is.null(comp_ids)){
    args <- paste('batch_id=', batch_id,"" sep='')
  }else{
    args <- paste('hit_setting=', HITsetting, '&ids=', paste(comp_ids,collapse=','), sep='')
  }
  myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                     args, sep=''))
  mytry <- try(out<-fromJSON(rawToChar(as.raw(myget$content))))
  if(class(mytry) == "try-error"){
    out <- NULL
  }
  return(out)
}
