#' Find/create documents and retrieve IDs
#'
#' Reads in text, posts the text to the server, and returns the document IDs after saving the result to a new table. If text already exists on the server, the associated ID is returned (there will never be duplicated text).
#'
#'
#' @param email The researcher's email associated with the SentimentIt account.
#' @param password The researcher's password associated with the SentimentIt account.
#' @param read_documents_from The file path the data will be drawn form, or a vector of text.
#' @param write_documents_to The file path to write the original data, merged with the document IDs. Default is NULL and the results will not be saved, but only returned. For best functionality specify a csv.
#' @param what Argument passed to scan() function. Default is character. Only needed when index is NULL.
#' @param sep Argument passed to read.table() function. Default is line break. Only needed when index is not NULL.
#' @param quiet Argument passed to scan(). Default is TRUE. Only needed when index is NULL.
#' @param index The index number of the table to extract the text from, or the name of the column. Default is NULL, indicating the text was not sent in a table.
#' @param which_source Source used within SentimentIt server assoicated with document uploads. Only used for later reference. Default is apiR.
#' @param ... Additional arguments passed to either scan() or read.table() depending on type of data used
#' 
#' @return A data frame with the provided data and the IDs appended
#'
#' @examples
#'
#' \dontrun{
#' data(reviews)
#'
#' docInfo <- readText(email = 'researcher@school.edu', password = 'uniquePassword', read_documents_from = reviews, write_documents_to = "ReviewsWithIds.csv", index = 'Review')
#' }
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @author David Carlson
#' @rdname readText
#' @export
readText <- function(email, password, read_documents_from, write_documents_to=NULL, what='character', sep='\n', quiet=TRUE, index=NULL, which_source='apiR', ...){
  if(!is.null(index)){
    if(!is.character(read_documents_from)){
      hold.table = read_documents_from
    }else{
      hold.table <- read.table(file=read_documents_from, sep=sep, ...)
    }
    textToSend <- hold.table[,index]
  }else textToSend <- scan(file=read_documents_from, what=what, sep=sep, quiet=quiet, ...)
  auth_token <- sentimentIt::authenticate(email, password)
  args <- mapply(function(x,y) list(text=x, source=y), textToSend, which_source, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  args <- toJSON(list("email"=email, "auth_token"=auth_token, "documents"=args), auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/documents/find_or_create.json',
                 body = args, content_type_json(),
                 encode='json')
  ids <- unlist(fromJSON(rawToChar(as.raw(mypost$content))))
  textToSend <- gsub('\t',' ', textToSend)
  if(is.null(index)){
    if(!is.null(write_documents_to)) write.table(cbind(textToSend, ids), write_documents_to, sep='\t', row.names=FALSE)
    return(as.data.frame(cbind(textToSend, ids), stringsAsFactors=FALSE))
  }else{
    hold.table[,index] <- textToSend
    if(!is.null(write_documents_to)) write.table(cbind(hold.table, ids), write_documents_to, sep='\t', row.names=FALSE)
    return(as.data.frame(cbind(hold.table, ids), stringsAsFactors=FALSE))
  }
}
