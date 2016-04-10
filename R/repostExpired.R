repostExpired <- function(batch_id){
  require(jsonlite)
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  for(i in batch_id){
    PUT('https://sentimentit.herokuapp.com/api/batches/',i,'/repost_expired.json')
  }
}
repostExpired(2)
