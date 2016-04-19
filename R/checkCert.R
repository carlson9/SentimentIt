#' Checks certifications for workers.
#'
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to check for certification.
#'
#' @return Status of the workers' certifications.  
#'
#' @author David Carlson
#'
#' @rdname checkCert
#' @export
checkCert <- function(cert, workers){
  out <- vector()
  args <- list(certification = cert, workers = workers)
  args <- toJSON(args, auto_unbox=TRUE)
  for (i in workers){
  URL <- paste0("http://sentimentit.herokuapp.com/api/certifications/", as.character(cert),
          "/turk_workers/", as.character(workers[i]), ".json")
  mypost <- POST(URL, body = args, content_type_json(),
          encode='json')
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  return(out)
  }
  }
}
#' @export
