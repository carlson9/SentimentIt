#' Create Tasks Timed
#'
#' Creates batches, and waits a certain
#'
#' @param batches Vector of Batch numbers to check
#' @param certone The name of the certification wanted for the workers.
#' @param certtwo The name of the certification wanted to be removed from the workers.
#' @param time_per Time per batch to post on to Mechanical Turk
#' @param mintime The earliest time in the morning to post comparisons to workers
#' @param maxtime The latest time at night to post comparisons to workers
#' @param certone Certification to give
#' @param certtwo Certification to revoke
#' @param checkWorkersAt batch positions to check workers(i.e. batches 1, 3, 5)
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
#' @return out IDs for batches of comparisons
#' @author Jacob M. Montgomery
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createTasks}}, \code{\link{createTasksBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#' @rdname createTasksTimed
#'
#' @export
.createTaskstimed <- function(batches, time_per, mintime,
                            maxtime, certone, certtwo, checkWorkersAt=NULL,
                            hierarchy_data=NULL, hierarchy_var=NULL,
                            returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                            n.questions=50, plot_hist=FALSE, file_path=NULL,
                            chains=3, iter=2500, seed=1234, n.cores=3){
  out <- vector()
  banned_workers <- vector()
  for(i in batches){
    checkTime(mintime, maxtime)
    x <- createTasks(batch_id=i)
    out <- c(out, x)
    Sys.sleep(time_per*3600)
    if(i %in% batches[checkWorkersAt]) {
       givetakeCert(certone, certtwo, stanWrapper(data=batches[1:length(out)],
                                                  hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                                                  returnFit=returnFit, cut_point=cut_point, cut_proportion=cut_proportion,
                                                  n.questions=n.questions, plot_hist=plot_hist, file_path=file_path,
                                                  chains=chains, iter=iter, seed=seed, n.cores=n.cores)[[1]])
    }
  }
  return(out)
}
checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}
