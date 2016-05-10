
#' @export
.createTasksBatch <- function(batches, certone, certtwo, min_time=9,
                   max_time=22, rate=1/3, threshold=5, checkWorkersAt=NULL,
                   hierarchy_data=NULL, hierarchy_var=NULL,
                   returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                   n.questions=50, plot_hist=FALSE, file_path=NULL,
                   chains=3, iter=2500, seed=1234, n.cores=3){
  if(!is.character(certone) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(certtwo) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(certone == certtwo){
    stop("The certifications you are giving and taking away cannot be made the same.")
  }
  if(!is.numeric(batches) | nchar(batches)<1){
    stop("The batch numbers must be numerical digits and non-blank.")
  }
  out <- vector()
  for(i in batches){
    checkTime(min_time, max_time)
    x <- createTasks(batch_id=i)
    out[i] <- x
    done <- FALSE
    current <- as.numeric(format(Sys.time(), "%M"))
      while(!done){
        Sys.sleep(rate*3600)
        status <- batchStatus(batches[i])
        done <- (((status$submitted_count - status$completed_count) <= threshold) | (as.numeric(format(Sys.time(), "M%")) - current > 240))
      }
    if(i %in% batches[checkWorkersAt]) {
      givetakeCert(certone, certtwo, stanWrapper(data=batches[1:length(out)],
                                                 hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                                                 returnFit=returnFit, cut_point=cut_point, cut_proportion=cut_proportion,
                                                 n.questions=n.questions, plot_hist=plot_hist, file_path=file_path,
                                                 chains=chains, iter=iter, seed=seed, n.cores=n.cores)[[1]])
  }
  return(out)
}
#' @export
checkTime <- function(min_time, max_time){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=max_time) Sys.sleep((24-current+min_time)*3600)
  if(current<=min_time) Sys.sleep((min_time-current)*3600)
}

