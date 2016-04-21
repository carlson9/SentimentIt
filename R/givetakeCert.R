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
  if(!is.character(certone) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(certtwo) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(certone == certtwo){
    stop("The certifications you are giving and taking away cannot be made the same.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank worker id and one made of characters.")
  }
  createCert(certone, workers)
  revokeCert(certtwo, workers)
}
