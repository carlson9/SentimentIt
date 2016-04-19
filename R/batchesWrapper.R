#' Batches Wrapper
#'
#' Creates batches, and waits a certain 
#'
#' @param batches Vector of Batch numbers to check
#'
#' @return batches
#' @author Jacob M. Montgomery
#' @note 
#' @examples
#'
#' @rdname createHITSTimed
#' @export
batchesWrapper <- function(hit_setting_id, num_batches, ids, number_per, question,checkWorkersAt=NULL,
                           rest_time=60, time_per=1,mintime=8,maxtime=22, certone=NULL, certtwo=NULL,
                           path=NULL, name=NULL, idsAsComps=FALSE){
   batches <- createBatches(hit_setting_id = hit_setting, num_batches = num_batches)
  # creates comparisons attached to the created batches. 
  makeCompsSep(ids=ids, number_per=number_per, batches=batches, question=question,
               path=path,name=name,idsAsComps=idsAsComps)
  Sys.sleep(rest_time)
  # Create HITS for each of the created batches
  createHITSTimed(batches=batches, time_per=1,mintime=8,maxtime=22,
                  checkWorkersAt=batches[checkWorkersAt], certone=certone,certtwo=certtwo)
  return(batches)
}