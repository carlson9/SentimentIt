#' sentimentIt
#'
#' A wrapper function of batchesWrapper and repostExpired

#' @param readDocumentsFrom Where the text will be drawn from
#' @param task_setting_id ID of HIT setting to use
#' @param question the question the worker will see once the worker selects the Task
#' @param waitToRepost Default is 2 hours
#' @param timed Tasks are replaced by time, not batch status
#' @param writeDocumentsTo Where to send the text to be reviewed to
#' @param what The text to be sent and used in the data frame
#' @param sep Where to separate text by line
#' @param quiet If true, this does not print the amount of items read prior
#' @param index The index number
#' @param which_source What type of file is the text being drawn from
#' @param number_per How many documents per batch to be compared
#' @param per_batch number of comparisons perbatch desired
#' @param path File path
#' @param name File name
#' @param time_per Default is 1
#' @param mintime Default is 8
#' @param maxtime Default is 22
#' @param certone Default is NULL
#' @param certtwo Default is NULL
#' @param checkWorkersAt Default is NULL
#' @param rest_time Default is 60
#' @param rate Default is 1/3
#' @param threshold Default is 5
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
#' @param returnStan Default is TRUE
#' @param stanFile Default is NULL
#' @param returnData Default is TRUE
#' @param dataFile Default is NULL
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
#' @rdname sentimentIt
#' @export
sentimentIt <- function(readDocumentsFrom, task_setting_id, question, waitToRepost = 2, 
                        timed=TRUE, writeDocumentsTo=NULL, what='character', sep='\n', 
                        quiet=TRUE, index=NULL, which_source='apiR',
                        number_per=20, per_batch=1000, path=NULL,
                        name=NULL, time_per=1, mintime=8, maxtime=22, 
                        certone=NULL, certtwo=NULL,
                        checkWorkersAt=NULL,
                        rest_time=60, rate=1/3, threshold=5,
                        hierarchy_data=NULL, hierarchy_var=NULL,
                        returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                        n.questions=50, plot_hist=FALSE, file_path=NULL,
                        chains=3, iter=2500, seed=1234, n.cores=3, 
                        returnStan=TRUE, stanFile=NULL, returnData=TRUE, 
                        dataFile=NULL, ...){

  batches <- batchesWrapper(readDocumentsFrom=readDocumentsFrom, task_setting_id=task_setting_id,
                            question=question, timed=timed, writeDocumentsTo=writeDocumentsTo,
                            what=what, sep=sep, quiet=quiet, index=index,
                            which_source=which_source, number_per=number_per,
                            per_batch=per_batch, path=path, name=name,
                            time_per=time_per, mintime=mintime, maxtime=maxtime,
                            certone=certone, certtwo=certtwo,
                            checkWorkersAt=checkWorkersAt, rest_time=rest_time,
                            rate=rate, threshold=threshold,
                            hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                            returnFit=returnFit, cut_point=cut_point,
                            cut_proportion=cut_proportion, n.questions=n.questions,
                            plot_hist=plot_hist, file_path=file_path,
                            chains=chains, iter=iter, seed=seed, n.cores=n.cores, ...)

  repostExpired(batches)
}
