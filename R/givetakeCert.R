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
#' @export
givetakeCert <- function(certone, certtwo, workers){
  createCert(certone, workers)
  revokeCert(certtwo, workers)
}
#' @export
