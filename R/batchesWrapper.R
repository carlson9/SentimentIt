#' Batches Wrapper
#'
#' A wrapper function of createBatches, readText
#'
#' @param pathFrom What file path the data will be drawn form, or actual data
#' @param hit_setting_id ID of HIT setting to use
#' @param question the question the worker will see once the worker selects the HIT
#' @param timed If True, HITS will be created by time, if not by batch
#' @param pathTo Where to send the text to be reviewed to.
#' @param what The text to be sent and used in the data frame.
#' @param sep Where to separate text by line.
#' @param quiet If true, this does not print the amount of items read prior.
#' @param index The index number
#' @param which_source What type of file is the text being drawn from.
#' @param number_per number of comparisons desired
#' @param per_batch number of comparisons perbatch desired
#' @param path File path: if NULL, file will be stored in working direcotry
#' @param name File name: If NULL, file will be named pairwise.Rdata
#' @param time_per Time per batch to post on to Mechanical Turk
#' @param mintime The earliest time in the morning to post comparisons to workers
#' @param maxtime The latest time at night to post comparisons to workers
#' @param certone Certification to give
#' @param certtwo Certification to revoke
#' @param checkWorkersAt batch positions to check workers(i.e. batches 1, 3, 5)
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy (defalt is NULL)
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy (defalt is NULL)
#' @param returnFit Return a fit object if TRUE (default is FALSE)
#' @param plot If TRUE, create a histogram with a rug plot (default is FALSE)
#' @param file Save the histogram to path and file name specified (default is NULL)
#' @param chains The number of chains (default is 3)
#' @param iter The number of iteration (default is 2500)
#' @param seed Set seed (default is 1234)
#' @param n.cores Number of cores to be used in stan fit (default is 3)
#'
#' @return batches A vector of batch numbers that have been created
#'
#' @author Jacob M. Montgomery
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{createHITSTimed}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#' @example
#' \dontrun{
#' batchesWrapper(timed=TRUE, hit_setting_id=2,question=,pathfrom="/dropbox/documents/questions")
#'}
#'
#' @rdname batchesWrapper
#'
#' @export
batchesWrapper <- function(pathfrom, hit_setting_id, question,
                           timed=TRUE, pathto=NULL, what="character",
                           sep="\n", quiet=TRUE,
                           index=NULL, which_source="apiR",
                           number_per=20, per_batch=1000,
                           path=NULL, name=NULL,
                           time_per=1, mintime=9, maxtime=22,
                           certone=NULL, certtwo=NULL,
                           checkWorkersAt=NULL,
                           rest_time=60, rate=1/3, threshold=5,
                           hierarchy_data=NULL, hierarchy_var=NULL,
                           returnFit=FALSE, plot=FALSE, file=NULL,
                           chains=3, iter=2500,
                           seed=1234, n.cores=3, ...){

  # read text into API
  if(is.null(pathto)){
  textDoc <- readText(pathfrom=pathfrom, pathto=pathto, what=what, sep=sep, quiet=quiet,
                  index=index, which_source=which_source, ...)
  } else {
    readText(pathfrom=pathfrom, pathto=pathto, what=what, sep=sep, quiet=quiet,
             index=index, which_source=which_source, ...)
    textDoc <- read.csv(paste(pathto,".csv",sep="")) #I am unsure what file type read Text outputs
  }
  # num batches is created from length of ids * number of comparisons / number per batch
  num_batches <- length(textdoc$ids) * number_per / per_batch
  batches <- createBatches(hit_setting_id=hit_setting, num_batches=num_batches)
  # creates comparisons attached to the created batches.
  makeCompsSep(ids=textDoc$ids, number_per=number_per, batches=batches, question=question,
               path=path, name=name)
  Sys.sleep(rest_time)

  # Create HITS for each of the created batches
  if(timed){
   createHITSTimed(batches=batches, time_per=time_per, mintime=mintime, maxtime=maxtime,
                    checkWorkersAt=batches[checkWorkersAt], certone=certone, certtwo=certtwo,
                   hierarchy_data=hierarchy_data,  hierarchy_var=hierarchy_var, returnFit=returnFit,
                   plot=plot, file=file, chains=chains, iter=iter, seed=seed, n.core=n.core)
  } else{
    createHITSBatch(batches=batches, min_time=min_time, max_time=max_time,
                           rate=rate, threshold=threshold, checkWorkersAt=batches[checkWorkersAt])
  }
  return(batches)
}

