#' Changes certifications for workers.
#'
#' @param certone The name of the certification wanted for the workers.
#' @param certtwo The name of the certification wanted to be removed from the workers.
#' @param workers The workers you want to use for certification.
#'
#' @return Changes the workers' certifications.  
#'
#' @author David Carlson
#'
#' @rdname givetakeCert
#' 
#' @importFrom pgk sentimentIt
#' 
#' @export
createCert <- function(certone, certtwo, workers){
  require(jsonlite)
  require(httr)
  revokeCert(certone, workers)
  createCert(certtwo, workers)
}
#' @export
