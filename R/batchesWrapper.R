batchesWrapper <- function(hit_setting_id, num_batches, ids, number_per, question ){
  batches <- createBatches(hit_setting_id = hit_setting, num_batches = num_batches)
  makeCompsSep(ids=ids,number_per = number_per,batches = batches,question=question)
}