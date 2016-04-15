batchesWrapper <- function(hit_setting_id, num_batches, ids, number_per, question, rest_time=10 ){
   batches <- createBatches(hit_setting_id = hit_setting, num_batches = num_batches)
  # creates comparisons attached to the created batches. 
  makeCompsSep(ids=ids, number_per=number_per, batches=batches, question=question)
  Sys.sleep(rest_time)
  # Create HITS for each of the created batches
  createHITSTimed(batches=batches, time_per=1,mintime=8,maxtime=22,checkWorkersAt=batches)
}