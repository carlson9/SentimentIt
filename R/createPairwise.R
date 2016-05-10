#' Creates documents with two texts to be compared.
#'
#' @param ids The id numbers of the texts you want to use.
#' @param number_per How many documents per batch to be compared.
#'
#' @return Creates a document with the texts to be compared.
#'
#' @author David Carlson
#'
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}
#' }
#' @note This function requires the usage of the plyr package.
#' @rdname createPairwise
#' @export
createPairwise <- function(ids, number_per){

  if(!is.numeric(ids)){
    stop("ids should be numeric")
  }

  if(!is.numeric(number_per)){
    stop("number_per should be numeric")
  }

  documents <- unique(as.numeric(ids))
  # This code sets up the random pairwise comparisons
  pairwise<-cbind(rep(documents, (number_per+1)%/%2), matrix(replicate((number_per+1)%/%2, sample(documents)), ncol=1))
  duplicates<-pairwise[which(pairwise[,1]==pairwise[,2]),]

  # some ugly code to keep documents from being compared with themselves
  if(!is.null(nrow(duplicates))){
    while(nrow(duplicates)>1){
      duplicates[,1]<-duplicates[sample(1:nrow(duplicates)),1]
      pairwise[which(pairwise[,1]==pairwise[,2]),]<-duplicates
      duplicates<-matrix(pairwise[which(pairwise[,1]==pairwise[,2]),], ncol=2)
    }
    if(nrow(duplicates)==1){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }else{
    if(is.vector(duplicates)){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }
  return(pairwise)
}
#' @export
