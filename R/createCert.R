#' Creates certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return Certifications of the workers.  
#'
#' @author David Carlson
#'
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- c("a204", "a206", "a208", "a207")
#' creation <- createCert(x, y)
#' }
#' @rdname createCert
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createHITSTimed}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
createCert <- function(cert, workers){
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank certification and one made of characters.")
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
