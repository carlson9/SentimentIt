#' Checks certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to check for certification.
#'
#' @return Status of the workers' certifications.  
#'
#' @author David Carlson
#'
#' @rdname checkCert
#' @export
checkCert <- function(cert, workers){
  mypost <- GET(paste0("https://sentimentit.herokuapp.com/api/certifications/", as.character(cert),
                       "/turk_workers/", as.character(workers), ".json"))
  return(fromJSON(rawToChar(as.raw(mypost$content)))$allowed)
}
