#' Revokes certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return The number of workers revoked.  
#'
#' @author David Carlson
#'
#' @rdname revokeCert
#' @export
createCert <- function(cert, workers){
  out <- vector()
  args <- list(certification = cert, workers = workers)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/certifications/revoke.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
