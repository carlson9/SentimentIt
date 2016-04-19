timedWrapper <- function(hit_setting_id, num_batches, ids, number_per, question,checkWorkersAt=NULL,
                         rest_time=60, time_per=1,mintime=8,maxtime=22,path=NULL, name=NULL, 
                         idsAsComps=FALSE, certone, certtwo){
  batches <- batchesWrapper(hit_setting_id=hit_setting_id,num_batches=num_batches,ids=ids,
                            question=question,checkWorkersAt=checkWorkersAt,rest_time=rest_time,
                            time_per=time_per, mintime=mintime,maxtime=maxtime,
                            certone=certone,certtwo=certtwo,path=path,idsAsComps=idsAsComps)
  repostExpired(batches)
}