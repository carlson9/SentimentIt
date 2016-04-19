#' Create Comparisons
#'
#' Create comparisons using multiple batch numbers.
#'
#' @param ids The id numbers of the texts you want to use.
#' @param number_per How many documents per batch to be compared.
#' @param batches The number of batches to be made.
#' @param question Where to separate text by line.
#' @param per_batch If true, this does not print the amount of items read prior.
#' @param path File path
#' @param name File name
#' @param idsAsComps IDs as comparison
#'
#' @return out a table with the text and correspondings ID's that have been sent.
#'
#' @author David Carlson
#' @note Makes use of the createPairwise function. Also requires the jsonlite and httr packages.
#' @rdname makeCompsSep
#' @export
timedWrapper <- function(hit_setting_id, num_batches, number_per, question,checkWorkersAt=NULL,
                         rest_time=60, time_per=1, mintime=8, maxtime=22, path=NULL, name=NULL, 
                         idsAsComps=FALSE, certone, certtwo, pathfrom, pathto=NULL, what='character', 
                         sep='\n', quiet=TRUE, index=NULL, which_source='apiR', ...){
  batches <- batchesWrapper(hit_setting_id=hit_setting_id, num_batches=num_batches, 
                            pathfrom=pathfrom, pathto=pathto, what=what, 
                            sep=sep, quiet=quiet, index=index, which_source=which_source, number_per=number_per,
                            question=question, checkWorkersAt=checkWorkersAt,rest_time=rest_time,
                            time_per=time_per, mintime=mintime,maxtime=maxtime,
                            certone=certone,certtwo=certtwo,path=path, name=name, idsAsComps=idsAsComps)
  repostExpired(batches)
}