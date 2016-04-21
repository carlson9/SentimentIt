#' Creates certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return Certifications of the workers.  
#'
#' @author David Carlson
#'
#' @rdname createCert
#' @export
createCert <- function(cert, workers){
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters."
  }
  out <- vector()
  args <- list(certification = cert, workers = workers)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/certifications/create.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  return(out)
}
#' @export
