#' Repost expired tasks from a batch
#' 
#' This will repost all of the expired tasks from the vector of batch IDs. It is common for a few tasks in a batch to be overlooked and remain incomplete.
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param batch_id ID of batch to check
#'
#' @return out reposted count
#' @author David Carlson
#' @examples
#' \dontrun{
#' repostExpired(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids)
#' }
#' @rdname repostExpired
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
repostExpired <- function(email, password, batch_id){
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be numeric")
  }
  auth_token <- sentimentIt::authenticate(email, password)
  args <- list(email = email, auth_token = auth_token)
  args <- toJSON(args, auto_unbox=TRUE)
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  for(i in batch_id){
    myput<-PUT(paste0('https://www.sentimentit.com/api/batches/',i,'/repost_expired'),
        body = args, content_type_json(),
        encode='json')
  }
}
