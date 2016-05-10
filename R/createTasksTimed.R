
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
