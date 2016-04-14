#' Create new batches
#' 
#' This function creates new batches with a HIT setting.
#' 
#' @param ids ID of HIT setting to use
#' @param num_batches number of batches to create using the HIT setting
#'
#' @return out ID of batches created
#' @author David Carlson
#' @examples
#'
#' createHITS(ids=10,HITsetting=2)
#' createHITS(batch_id=204)
#' @rdname createBatches
#' @export
createBatches <- function(hit_setting_id, num_batches=1){
  if(!is.numeric(hit_setting_id)){
    stop("hit_setting_id needs to be a numeric")
  }
  batch_ids <- vector()
  for(i in 1:num_batches){
    args <- paste("hit_setting_id=", hit_setting_id, sep="")
    myget <- POST(paste('http://sentimentit.herokuapp.com/api/batches.json?',
                       args, sep=''))
    batch_ids[i] <- fromJSON(rawToChar(as.raw(myget$content)))$id
  }
  return(batch_ids)
}
