#' Writes out text.
#'
#' Find/create documents and retrieve ids
#'
#' @param pathFrom What file path the data will be drawn form, or actual data
#' @param pathTo Where to send the text to be reviewed to.
#' @param what The text to be sent and used in the data frame.
#' @param sep Where to separate text by line.
#' @param quiet If true, this does not print the amount of items read prior.
#' @param index The index number
#' @param which_source What type of file is the text being drawn from.
#' 
#' @return List of ids corresponding to documents sent
#'
#' @author David Carlson
#' @rdname readText
#' @export
readText <- function(pathfrom, pathto=NULL, what='character', sep='\n', quiet=TRUE, index=NULL, which_source='apiR', ...){
  if(is.null(pathfrom)){
    stop("You must input a path from which the data will be taken.")
  }
  if(!is.null(index)){
    if(!is.character(pathfrom)){
      hold.table = pathfrom
    } else {
    hold.table <- read.table(file=pathfrom, sep=sep, ...)
    }
    textToSend <- hold.table[,index]
  }else textToSend <- scan(file=pathfrom, what=what, sep=sep, quiet=quiet, ...)
  args <- mapply(function(x,y) list(text=x, source=y), textToSend, which_source, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  args <- toJSON(list("documents"=args), auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/documents/find_or_create.json',
                 body = args, content_type_json(),
                 encode='json')
  ids <- unlist(fromJSON(rawToChar(as.raw(mypost$content))))
  textToSend <- gsub('\t',' ', textToSend)
  if(is.null(index)){
    if(is.null(pathto)){
      return(cbind(textToSend, ids))
    }else write.table(cbind(textToSend, ids), pathto, sep='\t', row.names=FALSE)
  }else{
    hold.table[,index] <- textToSend
    if(is.null(pathto)){
      return(cbind(hold.table, ids))
    }else write.table(cbind(hold.table, ids), pathto, sep='\t', row.names=FALSE)
  }
}
