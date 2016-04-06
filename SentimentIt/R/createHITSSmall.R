#' Views the hits and data of reviews of two documents.
#'
#' @param comp_ids The id numbers of the texts you want to use.
#' @param HITsetting HITsetting
#' @param batch_id Which batch will the reviews be taken from.
#'
#' @return Creates a document with the texts to be compared.
#'
#' @author David Carlson
#'
#' @note This function requires the usage of the httr and jsonlite packages.
#' @rdname createHITSSmall
#' @export
createHITSSmall<-function(comp_ids=NULL, HITsetting=9, batch_id){
  require(httr)
  require(jsonlite)
  if(is.null(comp_ids)){
    args <- paste('batch_id=', batch_id, sep='')
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
#' @export
