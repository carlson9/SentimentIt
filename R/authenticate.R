#' Authenticates researcher and returns authorization token. Primarily used internally but can be used if the researcher wishes to use the API directly.
#'
#' @param email The email used by the researcher to register with SentimentIt.
#' @param password The password associated with the account.
#'
#' @return auth_token Character for the authorization token, which expires after two hours.  
#'
#' @author David Carlson
#'
#' @rdname authenticate
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @export
authenticate <- function(email, password){
  args <- list(email = email, password = password)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/sessions.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- fromJSON(rawToChar(as.raw(mypost$content)))
  if(!out$success){
    email <- readline('Invalid email or password. Re-enter email: ')
    password <- readline('Re-enter password: ')
    return(authenticate(email, password))
  }
  return(out$auth_token)
}

