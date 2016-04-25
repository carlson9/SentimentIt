#' Create New Hits for comparisons
#' 
#' This function posts batches to Mechanical Turk as HITS
#' 
#' @param ids Comma-separated list of comparison ids for which to create hits
#' @param HITsetting id of the HIT Setting to use for the created HITs
#' @param batch_id id of the Batch of comparisons
#'
#' @return out ID for batch of comparisons
#' @author David Carlson
#' @examples
#' 
#' \dontrun{
#' createHITS(ids=10,HITsetting=2)
#' createHITS(batch_id=204)
#' }
#' @rdname createHITS
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
createHITS <- function(ids=NULL, HITsetting=NULL, batch_id){
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

