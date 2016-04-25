#' Timed Wrapper
#'
#' A wrapper function of batchesWrapper and repostExpired
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
#' @return
#'
#' @author David Carlson
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{createHITSTimed}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#'
#' @rdname timedWrapper
#'
#' @export
timedWrapper <- function(hit_setting_id, num_batches=1,
                         pathfrom, pathto=NULL, what='character', sep='\n', quiet=TRUE,
                         index=NULL, which_source='apiR',
                         number_per, batches, question, per_batch=1000, path=NULL,
                         name=NULL, idsAsComps=FALSE,
                         time_per=1, mintime=8, maxtime=22, certone=NULL, certtwo=NULL,
                         checkWorkersAt=NULL,
                         rest_time=60, ...){

  batches <- batchesWrapper(hit_setting_id=hit_setting_id, num_batches=num_batches,
                            pathfrom=pathfrom, pathto=pathto, what=what, sep=sep, quiet=quiet,
                            index=index, which_source=which_source,
                            number_per=number_per, batches=batches, question=question,
                            per_batch=per_batch, path=path, name=name, idsAsComps=idsAsComps,
                            time_per=time_per, mintime=mintime, maxtime=maxtime,
                            certone=certone, certtwo=certtwo, checkWorkersAt=NULL,
                            rest_time=60, ...)

  repostExpired(batches)
}
