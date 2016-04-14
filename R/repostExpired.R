#' Repost expired HITS from a batch
#' 
#' This function takes in expired batch_ids and reposts them to create new comparisons.
#' 
#' @param batch_id ID of batch to check
#'
#' @return out reposted count
#' @author David Carlson
#' @examples
#'
#' repostExpired(batch_id=2)
#' @rdname repostExpired
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
}

