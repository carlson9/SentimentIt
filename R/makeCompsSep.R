#' Create Comparisons
#'
#' Creates separate comparisons using document IDs. This outputs a file with a table of text
#' and corresponding ids for Mechanical Turk comparisons.
#'
#' @param ids numerical IDs for the documents
#' @param number_per number of comparisons desired
#' @param batches batch IDs to be used for the Tasks
#' @param question the question the worker will see once the worker selects the Task
#' @param per_batch number of comparisons perbatch desired
#' @param path File path: if NULL, file will be stored in working direcotry
#' @param name File name: If NULL, file will be named pairwise.Rdata
#' @param idsAsComps an indicator as to whether or not the IDs provided refer 
#'                   to comparison IDs rather than document IDs
#'
#' @return out a table with the text and correspondings ID's that have been sent.
#'
#' @author David Carlson
#' @example 
#' 
#' \dontrun {
#' docInfo <- read.table("ReviewsWithIds",header=TRUE)
#' makeCompsSep(ids=docInfo[,'ids'], number_per=10, batches=batch_ids,
#'              question=’Below is text taken from two movie reviews.Please 
#'              choose the text that you think comesfrom the most positive review’,
#'              path='Comparisons/', name='first10')
#' }
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @rdname makeCompsSep
#' @export
makeCompsSep <- function(ids, number_per, batches, question, per_batch=1000, 
                         path=NULL, name=NULL, idsAsComps=FALSE){
  if(!idsAsComps){
    pairwise <- createPairwise(ids, number_per)
    save(pairwise, file=paste0(path, 'pairwise', name, '.Rdata'))
  }else{
    pairwise <- ids
  }
  num_comps <- dim(pairwise)[1]
  if(length(batches)*per_batch<num_comps){
    print('Not enough batches supplied')
    return(NULL)
  }
  out <- vector()
  for(i in 1:(num_comps%/%per_batch)){
    args <- list(question=question, ids=pairwise[((i-1)*per_batch+1):(i*per_batch),], batch_id=batches[i])
    args <- toJSON(args, auto_unbox=TRUE)
    mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
    out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  }
  if(num_comps%%per_batch!=0){
    args <- list(question=question, ids=pairwise[((num_comps%/%per_batch)*per_batch+1):num_comps,], batch_id=batches[i+1])
    args <- toJSON(args, auto_unbox=TRUE)
    mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
    out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  }
  return(out)
}
