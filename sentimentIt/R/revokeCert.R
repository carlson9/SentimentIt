#' Revoke a certification for workers
#'
#' This function simply removes workers from the list of approved workers and they will no longer be able to perform the tasks requiring the certification.
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
#' revokeCert(email = 'researcher@school.edu', password = 'uniquePassword', cert = 'snippets', workers = ban_workers)
#' }
#'
#' @rdname revokeCert
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{signout}}
#' @export
revokeCert <- function(email, password, cert, workers){
 auth_token <- sentimentIt::authenticate(email, password)
 if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  out <- vector()
  args <- list(email = email, auth_token = auth_token, certification = cert, workers = list(workers))
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/certifications/revoke.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  }
