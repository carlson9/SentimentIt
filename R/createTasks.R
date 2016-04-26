#' Create New tasks for comparisons
#' 
#' This function posts batches to Mechanical Turk as Tasks
#' 
#' @param ids Comma-separated list of comparison ids for which to create tasks
#' @param Tasksetting id of the Task Setting to use for the created Tasks
#' @param batch_id id of the Batch of comparisons
#'
#' @return out ID for batch of comparisons
#' @author David Carlson
#' @examples
#' 
#' \dontrun{
#' createTasks(ids=10,Tasksetting=2)
#' createTasks(batch_id=204)
#' }
#' @rdname createTasks
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createTasks}}, \code{\link{createTasksBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}} 
#' }
#' @export
createTasks <- function(ids=NULL, Tasksetting=NULL, batch_id){
  if(!is.numeric(ids) | !is.numeric(Tasksetting) | !is.numeric(batch_id)){
    stop("All arguments need to be numeric.")
  }
  if(is.null(ids)){
    args <- paste('batch_id=', batch_id,"" ,sep='')
  }else{
    args <- paste('task_setting=', Tasksetting, '&ids=', paste(ids,collapse=','), sep='')
  }
  myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_tasks?',
                     args, sep=''))
  mytry <- try(out<-fromJSON(rawToChar(as.raw(myget$content))))
  if(class(mytry) == "try-error"){
    out <- 'error'
  }
  return(out)
}

