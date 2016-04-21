#' Batches Wrapper
#'
#' A wrapper function of createBatches, readText
#'
#'
#' @param hit_setting_id ID of HIT setting to use
#' @param num_batches number of batches to create using the HIT setting
#' @param pathFrom Where the text will be drawn from
#' @param pathTo Where to send the text to be reviewed to
#' @param what The text to be sent and used in the data frame
#' @param sep Where to separate text by line
#' @param quiet If true, this does not print the amount of items read prior
#' @param index The index number
#' @param which_source What type of file is the text being drawn from
#' @param number_per How many documents per batch to be compared
#' @param batches The number of batches to be made
#' @param question Where to separate text by line
#' @param per_batch If true, this does not print the amount of items read prior
#' @param path File path
#' @param name File name
#' @param idsAsComps IDs as comparison
#' @param time_per Defalt is 1
#' @param mintime Defalt is 8
#' @param maxtime Defalt is 22
#' @param certone Defalt is NULL
#' @param certtwo Defalt isNULL
#' @param checkWorkersAt Defalt is NULL
#' @param rest_time Defalt is 60
#'
#' @return batches A vector of batch numbers
#'
#' @author Jacob M. Montgomery
#'
#' @examples
#'
#' @seealso \code{\link{createBatches}}, \code{\link{readText}}, \code{\link{makeCompsSep}}, \code{\link{createHITSTimed}}
#'
#' @rdname batchesWrapper
#'
#' @export
batchesWrapper <- function(hit_setting_id, num_batches=1,
                           pathfrom, pathto=NULL, what='character', sep='\n', quiet=TRUE,
                           index=NULL, which_source='apiR',
                           number_per, batches, question, per_batch=1000, path=NULL,
                           name=NULL, idsAsComps=FALSE,
                           time_per=1, mintime=8, maxtime=22, certone=NULL, certtwo=NULL,
                           checkWorkersAt=NULL,
                           rest_time=60, ...){

  batches <- createBatches(hit_setting_id=hit_setting, num_batches=num_batches)

  ids <- readText(pathfrom=pathfrom, pathto=pathto, what=what, sep=sep, quiet=quiet,
                  index=index, which_source=which_source)

  # creates comparisons attached to the created batches.
  makeCompsSep(ids=ids, number_per=number_per, batches=batches, question=question,
               path=path, name=name, idsAsComps=idsAsComps)
  Sys.sleep(rest_time)

  # Create HITS for each of the created batches
  createHITSTimed(batches=batches, time_per=time_per, mintime=mintime, maxtime=maxtime,
                  checkWorkersAt=batches[checkWorkersAt], certone=certone, certtwo=certtwo)

  return(batches)
}
