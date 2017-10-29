#' Batches Wrapper
#'
#' A wrapper function of createBatches, readText
#'
#' @param readDocumentsFrom What file path the data will be drawn form, or actual data
#' @param task_setting_id ID of Task setting to use
#' @param question the question the worker will see once the worker selects the Task
#' @param timed If True, Tasks will be created by time, if not by batch
#' @param writeDocumentsTo Where to send the text to be reviewed to.
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
#' @param rest_time the amount of time, in seconds, to wait to post HITs after the comparisons are created. (Default is 60)
#' @param rate The rate by which the progress of a batch will be checked (default = 1/3 an hour).
#' @param threshold Point at which a batch is considered done (default = 5).
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy. (Default is NULL)
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy. (Default is NULL)
#' @param returnFit Return a fit object if TRUE. (Default is FALSE)
#' @param cut_point A cutoff point to classify posterior coefficients. The proportion of posterior coefficients below \code{cut_point} is used to determine outliers. (Default is 1)
#' @param cut_proportion A cutoff proportion of posterior coefficients below \code{cut_point}. If the proportion of posterior coefficients below \code{cut_points} is higher than \code{cut_proportion}, a worker will be considered as an outlier provided that she answers more than the number of questions in \code{n.questions}. (Default is 0.9)
#' @param n.questions The number of questions to consider in order to determine banned workers. (Default is 50)
#' @param plot_hist If TRUE, plot the histogram of workers with a rug plot. (Default is FALSE)
#' @param file_path Save the histogram to path and file name specified. (Default is NULL)
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Default is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return batches A vector of batch numbers that have been created
#'
#' @author Jacob M. Montgomery
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeComps}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @example
#' \dontrun{
#' batchesWrapper(timed=TRUE, task_setting_id=2,question=,readDocumentsFrom="/dropbox/documents/questions")
#'}
#'
#' @rdname batchesWrapper
#'
#' @export
batchesWrapper <- function(readDocumentsFrom, task_setting_id, question,
                           timed=TRUE, writeDocumentsTo=NULL, what="character",
                           sep="\n", quiet=TRUE,
                           index=NULL, which_source="apiR",
                           number_per=20, per_batch=1000,
                           path=NULL, name=NULL,
                           time_per=1, mintime=9, maxtime=22,
                           certone=NULL, certtwo=NULL,
                           checkWorkersAt=NULL,
                           rest_time=60, rate=1/3, threshold=5,
                           hierarchy_data=NULL, hierarchy_var=NULL,
                           returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                           n.questions=50, plot_hist=FALSE, file_path=NULL,
                           chains=3, iter=2500, seed=1234, n.cores=3, ...){

  # read text into API
  if(is.null(writeDocumentsTo)){
  textDoc <- readText(readDocumentsFrom=readDocumentsFrom, writeDocumentsTo=writeDocumentsTo, what=what, sep=sep, quiet=quiet,
                  index=index, which_source=which_source, ...)
  } else {
    readText(readDocumentsFrom=readDocumentsFrom, writeDocumentsTo=writeDocumentsTo, what=what, sep=sep, quiet=quiet,
             index=index, which_source=which_source, ...)
    textDoc <- read.csv(paste(writeDocumentsTo,".csv",sep="")) #I am unsure what file type read Text outputs
  }
  # num batches is created from length of ids * number of comparisons / number per batch
  num_batches <- ceiling(length(unique(textdoc$ids)) * number_per / per_batch / 2)
  batches <- createBatches(task_setting_id=task_setting, num_batches=num_batches)
  # creates comparisons attached to the created batches.
  makeComps(ids=textDoc$ids, number_per=number_per, batches=batches, question=question,
               path=path, name=name)
  Sys.sleep(rest_time)

  # Create Tasks for each of the created batches
  if(timed){
   .createTasksTimed(batches=batches, time_per=time_per, mintime=mintime, maxtime=maxtime,
                    checkWorkersAt=batches[checkWorkersAt], certone=certone, certtwo=certtwo,
                    hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                    returnFit=returnFit, cut_point=cut_point, cut_proportion=cut_proportion,
                    n.questions=n.questions, plot_hist=plot_hist, file_path=file_path,
                    chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  } else{

    .createTasksBatch(batches=batches, min_time=min_time, max_time=max_time,
                     rate=rate, threshold=threshold, 
                     checkWorkersAt=batches[checkWorkersAt],
                     hierarchy_data=NULL, hierarchy_var=NULL,
                     returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                     n.questions=50, plot_hist=FALSE, file_path=NULL,
                     chains=3, iter=2500, seed=1234, n.cores=3)
  }
  return(batches)
}

