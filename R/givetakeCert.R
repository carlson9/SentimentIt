#' Changes certifications for workers.
#'
#' @param certone The name of the certification wanted for the workers.
#' @param certtwo The name of the certification wanted to be removed from the workers.
#' @param workers The workers you want to use for certification.
#'
#' @return Changes the workers' certifications.  
#'
#' @author David Carlson
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- "ab2" 
#' z <- c("a204", "a206", "a208", "a207")
#' givetake <- givetakeCert(x, y, z)
#' }
#'
#' @rdname givetakeCert
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{createHITSTimed}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
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
