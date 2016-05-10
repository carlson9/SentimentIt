
#' @export
.timedWrapper <- function(readDocumentsFrom, task_setting_id, question,
                          timed=TRUE, writeDocumentsTo=NULL, what="character",
                          sep="\n", quiet=TRUE,
                          index=NULL, which_source="apiR",
                          number_per=20, per_batch=1000,
                          path=NULL, name=NULL,
                          time_per=1, mintime=9, maxtime=22,
                          certone=NULL, certtwo=NULL,
                          checkWorkersAt=NULL,
                          rest_time=60, rate=1/3, threshold=5,
                          hierarchy_data=NULL, hierarchy_var=NULL,
                          returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                          n.questions=50, plot_hist=FALSE, file_path=NULL,
                          chains=3, iter=2500, seed=1234, n.cores=3, ...){

  # use batchesWrapper function
  batches <- batchesWrapper(readDocumentsFrom=readDocumentsFrom, task_setting_id=task_setting_id,
                            question=question, timed=timed, writeDocumentsTo=writeDocumentsTo,
                            what=what, sep=sep, quiet=quiet, index=index,
                            which_source=which_source, number_per=number_per,
                            per_batch=per_batch, path=path, name=name,
                            time_per=time_per, mintime=mintime, maxtime=maxtime,
                            certone=certone, certtwo=certtwo,
                            checkWorkersAt=checkWorkersAt, rest_time=rest_time,
                            rate=rate, threshold=threshold,
                            hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                            returnFit=returnFit, cut_point=cut_point,
                            cut_proportion=cut_proportion, n.questions=n.questions,
                            plot_hist=plot_hist, file_path=file_path,
                            chains=chains, iter=iter, seed=seed, n.cores=n.cores, ...)

  # repost all of the expired Tasks from the vector of batch IDs based on batchesWrapper
  repostExpired(batches)
}
