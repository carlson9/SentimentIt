#' Create Comparisons
#'
#' Create comparisons using multiple batch numbers.
#'
#' @param ids The id numbers of the texts you want to use.
#' @param number_per How many documents per batch to be compared.
#' @param batches The number of batches to be made.
#' @param question Where to separate text by line.
#' @param per_batch If true, this does not print the amount of items read prior.
#' @param path File path
#' @param name File name
#' @param idsAsComps IDs as comparison
#'
#' @return out a table with the text and correspondings ID's that can be sent.
#'
#' @author David Carlson
#' @note Makes use of the createPairwise function. Also requires the jsonlite and httr packages.
#' @rdname makeCompsSep
#' @export
makeCompsSep <- function(ids, number_per, batches, question, per_batch=1000, path=NULL, name=NULL, idsAsComps=FALSE){
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
  require(jsonlite)
  require(httr)
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
