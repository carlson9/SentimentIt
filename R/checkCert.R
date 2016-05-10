#' Checks certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to check for certification.
#'
#' @return Status of the workers' certifications.  
#'
#' @author David Carlson
#'
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- c("a204", "a206", "a208", "a207")
#' check <- checkCert(x, y)
#' }
#'
#' @rdname checkCert
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @export
checkCert <- function(cert, workers){
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  mypost <- GET(paste0("https://sentimentit.herokuapp.com/api/certifications/", as.character(cert),
                       "/turk_workers/", as.character(workers), ".json"))
  return(fromJSON(rawToChar(as.raw(mypost$content)))$allowed)
}
