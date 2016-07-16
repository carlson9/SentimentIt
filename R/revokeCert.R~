
#' Revokes certifications for workers.
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return The number of workers revoked.  
#'
#' @author David Carlson
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- c("a204", "a206", "a208", "a207")
#' revocation <- revokeCert(x, y)
#' }
#'
#' @rdname revokeCert
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @export
revokeCert <- function(email, password, cert, workers){
 auth_token <- authenticate(email, password)
 if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  out <- vector()
  args <- list(email = email, auth_token = auth_token, certification = cert, workers = workers)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/certifications/revoke.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  }
