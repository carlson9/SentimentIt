#' sentimentIt
#'
#' A wrapper function of batchesWrapper and repostExpired
#'
<<<<<<< Updated upstream
#' @param pathFrom Where the text will be drawn from
#' @param task_setting_id ID of task setting to use
#' @param question Where to separate text by line
#' @param waitToRepost Time before reposting expired tasks
#' @param pathTo Where to send the text to be reviewed to
#' @param what The text to be sent and used in the data frame
#' @param sep Where to separate text by line
#' @param quiet If true, this does not print the amount of items read prior
#' @param index The index number
#' @param which_source What type of file is the text being drawn from
#' @param number_per How many documents per batch to be compared
#' @param per_batch If true, this does not print the amount of items read prior


#' @param timed HITS are replaced by time, not batch status
#' @param hit_setting_id ID of HIT setting to use
#' @param num_batches number of batches to create using the HIT setting
#' @param pathFrom Where the text will be drawn from
#' @param pathTo Where to send the text to be reviewed to
=======
#' @param timed Tasks are replaced by time, not batch status
#' @param hit_setting_id ID of Task setting to use
#' @param num_batches number of batches to create using the Task setting
#' @param readDocumentsFrom Where the text will be drawn from
#' @param writeDocumentsTo Where to send the text to be reviewed to
>>>>>>> Stashed changes
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
#' @seealso \code{\link{batchStatus}}, \code{\link{createTasksTimed}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createTasks}}, \code{\link{createTasksBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#'
#' @rdname timedWrapper
#' @export
sentimentIt <- function(timed, hit_setting_id, num_batches=1,
                         readDocumentsFrom, writeDocumentsTo=NULL, what='character', sep='\n', quiet=TRUE,
                         index=NULL, which_source='apiR',
                         number_per, batches, question, per_batch=1000, path=NULL,
                         name=NULL, idsAsComps=FALSE,
                         time_per=1, mintime=8, maxtime=22, certone=NULL, certtwo=NULL,
                         checkWorkersAt=NULL,
                         rest_time=60, ...){

  batches <- batchesWrapper(timed,hit_setting_id=hit_setting_id, num_batches=num_batches,
                            readDocumentsFrom=readDocumentsFrom, writeDocumentsTo=writeDocumentsTo, what=what, sep=sep, quiet=quiet,
                            index=index, which_source=which_source,
                            number_per=number_per, batches=batches, question=question,
                            per_batch=per_batch, path=path, name=name, idsAsComps=idsAsComps,
                            time_per=time_per, mintime=mintime, maxtime=maxtime,
                            certone=certone, certtwo=certtwo, checkWorkersAt=NULL,
                            rest_time=60, ...)

  repostExpired(batches)
}
