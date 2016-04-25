#' Repost expired HITS from a batch
#' 
#' This will repost all of the expired HITs from the vector of batch IDs, because once all
#' batches are complete it is common that a few HITS remain incomplete.
#' 
#' @param batch_id ID of batch to check
#'
#' @return out reposted count
#' @author David Carlson
#' @examples
#' \dontrun{
#' repostExpired(batch_id=2)
#' }
#' @rdname repostExpired
#' @seealso \code{\link{createHITSTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
repostExpired <- function(batch_id){
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  for(i in batch_id){
    PUT('https://sentimentit.herokuapp.com/api/batches/',i,'/repost_expired.json')
  }
  #TODO need to get a return value from the api.
}

