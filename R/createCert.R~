#' Grants certifications to workers.
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return Character indicating number of workers certified.  
#'
#' @author David Carlson
#'
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- c("a204", "a206", "a208", "a207")
#' creation <- createCert(email, password, x, y)
#' }
#' @rdname createCert
#' @seealso \code{\link{sentimentIt}} \code{\link{\authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
createCert <- function(email, password, cert, workers){
  auth_token <- authenticate(email, password)
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  out <- vector()
  if(length(workers)==1){
      args <- list(email = email, auth_token = auth_token, certification = cert, workers = list(workers))
  }else{
      args <- list(email = email, auth_token = auth_token, certification = cert, workers = workers)
  }
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/certifications/create.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, rawToChar(as.raw(mypost$content)))
  return(out)
}

