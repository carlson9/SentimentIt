#can either specify a pathto or function will return the data frame to be written - can specify which column if not a text file with text on each line
#curl -H "Content-Type: application/json" -X POST -d '[ {"text": "bar222", "source": "api"}, {"text": "bar2222", "source": "api"} ]' http://sentimentit.herokuapp.com/api/documents/find_or_create.json

readText <- function(pathfrom, pathto=NULL, what='character', sep='\n', quiet=TRUE, index=NULL, which_source='apiR', ...){
  if(!is.null(index)){
    hold.table <- read.table(file=pathfrom, sep=sep, ...)
    textToSend <- hold.table[,index]
  }else textToSend <- scan(file=pathfrom, what=what, sep=sep, quiet=quiet, ...)
  args <- mapply(function(x,y) list(text=x, source=y), textToSend, which_source, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  require(jsonlite)
  require(httr)
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

#curl -H "Content-Type: application/json" -X POST -d '{ "question": "Below is text taken from two movie reviews. Please choose the text that you think comes from the most positive review", "ids": [[ 4968, 4959 ]] }'http://sentimentit.herokuapp.com/api/comparisons/create.json
#{"ids":[24]}%




#number_per should be multiple of 2, but if not the current code will just make it one more per
#function takes doc ids, number of comparisons per, batch id (optional), and returns comparison ids
makeComps <- function(ids, number_per, batch_id=NULL,
                      question= 'Below is text taken from two movie reviews. Please choose the text that you think comes from the most positive review'){
  documents <- unique(as.numeric(ids))
  require(plyr)
  # This code sets up the random pairwise comparisons
  pairwise<-cbind(rep(documents, (number_per+1)%/%2), matrix(replicate((number_per+1)%/%2, sample(documents)), ncol=1))
  duplicates<-pairwise[which(pairwise[,1]==pairwise[,2]),]
  
  # some ugly code to keep documents from being compared with themselves
  if(!is.null(nrow(duplicates))){
    while(nrow(duplicates)>1){
      duplicates[,1]<-duplicates[sample(1:nrow(duplicates)),1]
      pairwise[which(pairwise[,1]==pairwise[,2]),]<-duplicates
      duplicates<-matrix(pairwise[which(pairwise[,1]==pairwise[,2]),], ncol=2)
    }	
    if(nrow(duplicates)==1){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }else{
    if(is.vector(duplicates)){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }
  require(jsonlite)
  require(httr)
  out <- vector()
  if(is.null(batch_id)){
    args <- list(question=question, ids=pairwise)
  }else{
    args <- list(question=question, ids=pairwise, batch_id=batch_id)
  }
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
#  browser()
  out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  return(out)
}

createPairwise <- function(ids, number_per){
  documents <- unique(as.numeric(ids))
  require(plyr)
  # This code sets up the random pairwise comparisons
  pairwise<-cbind(rep(documents, (number_per+1)%/%2), matrix(replicate((number_per+1)%/%2, sample(documents)), ncol=1))
  duplicates<-pairwise[which(pairwise[,1]==pairwise[,2]),]
  
  # some ugly code to keep documents from being compared with themselves
  if(!is.null(nrow(duplicates))){
    while(nrow(duplicates)>1){
      duplicates[,1]<-duplicates[sample(1:nrow(duplicates)),1]
      pairwise[which(pairwise[,1]==pairwise[,2]),]<-duplicates
      duplicates<-matrix(pairwise[which(pairwise[,1]==pairwise[,2]),], ncol=2)
    }	
    if(nrow(duplicates)==1){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }else{
    if(is.vector(duplicates)){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }
  return(pairwise)
}

# send ids, number_per, a vector of batches, how many per batch, the question,
# a path to save the pairwise, and an optional modifier to the file name
# if too many batches supplied will only use the necessary number
makeComps_sep <- function(ids, number_per, batches, question, per_batch=1000, path=NULL, name=NULL, idsAsComps=FALSE){
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

createHITSSmall<-function(comp_ids=NULL, HITsetting=9, batch_id){
	 require(httr)
  require(jsonlite)
    if(is.null(comp_ids)){
       args <- paste('batch_id=', batch_id, sep='') 
    }else{
       args <- paste('hit_setting=', HITsetting, '&ids=', paste(comp_ids,collapse=','), sep='')
    }
	myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                       args, sep=''))
	mytry <- try(out<-fromJSON(rawToChar(as.raw(myget$content))))
	if(class(mytry) == "try-error"){
	  out <- NULL
	}
    return(out)
}

# All times are in hours, but do not have to be integers

createHITStimed <- function(batches, time_per, mintime, maxtime){
  out <- vector()
  for(i in 1:length(batches)){
    checkTime(mintime, maxtime)
    args <- paste('batch_id=', batches[i], sep='') 
    myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                       args, sep=''))
    mytry <- try(out<-c(out, fromJSON(rawToChar(as.raw(myget$content)))))
    if(class(mytry) == "try-error"){
      out[i] <- 'error'
    }
    Sys.sleep(time_per*3600)
    #could alter above to check if batch is completed
  }
  return(out)
}

checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}

#curl -X GET -d 'hit_setting=1&ids=1,2' http://sentimentit.com/api/comparisons/create_hits
#creates HITs with comparison ids and HIT setting as arguments, return batch id

#################################################
#### I think this function is depricated ########
#################################################
createHITS <- function(comp_ids, HITsetting=2){
  iters <- length(comp_ids)
  num.loop <- iters%/%100
  require(httr)
  require(jsonlite)
  out <- vector()
  if(num.loop>0){
    for(i in 1:num.loop){
      args <- paste('hit_setting=', HITsetting, '&ids=', paste(comp_ids[((i-1)*100+1):(i*100)],collapse=','), sep='')
      myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                       args, sep=''))
      mytry <- try(fromJSON(rawToChar(as.raw(myget$content))))
      if(class(mytry) == "try-error"){
        out <- c(out, out[length(out)])
      }else{
        out <- c(out, fromJSON(rawToChar(as.raw(myget$content)))) 
      }
      Sys.sleep(30)
    }
  }
  if(iters%%100!=0){
    leftover <- (num.loop*100+1):iters
    args <- paste('hit_setting=', HITsetting, '&ids=', paste(comp_ids[((i-1)*100+1):(i*100)],collapse=','), sep='')
    myget <- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/create_hits?',
                       args, sep=''))
    mytry <- try(fromJSON(rawToChar(as.raw(myget$content))))
    if(class(mytry) == "try-error"){
      out <- c(out, out[length(out)])
    }else{
      out <- c(out, fromJSON(rawToChar(as.raw(myget$content)))) 
    }
  }
  #return character with range of batches
  return(paste(range(out), collapse=':'))
}

#slightly inefficient, but putting the write csv in this wrapper and calling the readText with pathto = NULL. wanted that function to operate without creating HITs, but makes more sense to have that return the ids for the wrapper
wrapper <- function(pathfrom, pathto=NULL, what='character', sep='\n', quiet=TRUE, index=NULL, which_source='apiR', HITsetting=2, number_per, question= 'Below is text taken from two movie reviews. Please choose the text that you think comes from the most positive review', ...){
  hold.table <- readText(pathfrom, what=what, sep=sep, quiet=quiet, index=index, which_source=which_source, ...)
  doc_ids <- hold.table[,'ids']
  comp_ids <- makeComps(doc_ids, number_per, question)
  batch_id <- createHITS(comp_ids, HITsetting)
  if(is.null(pathto)){
    return(cbind(hold.table, doc_ids, batch_id))
  }else write.table(cbind(hold.table, doc_ids, batch_id), pathto, sep='\t', row.names=FALSE)
}

readInBatch <- function(batchNumber, path="~/Dropbox/CATText/ImmigrationSurvey/"){
    require(httr)
    require(jsonlite)
    args <- paste('batch_id=', batchNumber, sep='')
    output<- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/download?', args, sep=''))
    write(rawToChar(as.raw(output$content)), file=paste0(path, "batch", batchNumber, ".csv"))
}

checkNotDone <- function(batches, pairwisepath='pairwise.Rdata'){
  temppath <- tempdir()
  output <- NULL
  for(batch in batches){
    readInBatch(batch, temppath)
    output <- rbind(output, read.csv(paste0(temppath, 'batch', batch, '.csv')))
  }
  load(pairwisepath)
  g <- output$document_id[seq(1,dim(output)[1],by=2)]
  h <- output$document_id[seq(1,dim(output)[1],by=2)+1]
  docs_wanted <- apply(pairwise, 1, paste, collapse=' ')
  needed_docs <- which(!(docs_wanted %in% apply(cbind(g,h),1,paste, collapse=' ') | docs_wanted %in% apply(cbind(h,g),1,paste, collapse=' ')))
  return(pairwise[needed_docs, ])
  #could modify function to check a single batch and a range of the pairwises
}


## work flow:
# make batches on GUI
# create comparisons with vector of batches using makeComps_sep(ids, number_per, batches, question, per_batch=1000, path=NULL, name=NULL)
# if above step is put into wrapper function need to add Sys.sleep to make sure they get up
# make HITs using createHITStimed(batches, time_per, mintime, maxtime)
# run checkNotDone(batches, pairwisepath='pairwise.Rdata')
# make new batch(es) on GUI
# use makeComps_sep() but set ids to pairwise missing and idsAsComps TRUE
# continue as needed


#takes a stan model fit as argument model, doc_ids (a vector), probability R, stage (l in the write-up), and num.posterior (number of rows of posterior to use, evenly spaced)
create_comps_update <- function(model, doc_ids, R=.5, stage=0, num.posterior=500){
  draws <- as.matrix(model, pars=c('sigma', 'lp__'), include=FALSE)
  draws <- draws[seq(1,dim(draws)[1], length.out = num.posterior),]
  comps <- matrix(NA, ncol=2, nrow=length(doc_ids)/2)
  iters <- length(doc_ids)/2
  for(i in 1:iters){
    r <- runif(1)
    doc1_index <- sample(x=1:length(doc_ids), size=1)
    doc1 <- doc_ids[doc1_index]
    doc_ids <- doc_ids[-doc1_index]
    if(r<R^stage|length(doc_ids==2)){
      doc2_index <- sample(x=1:length(doc_ids), size=1)
    }else{
      I_j <- apply(draws,1,function(x){
        ps <-arm::invlogit(x[doc1_index]-x[-doc1_index])
        return(ps*(1-ps))})
      doc2_index <- which.max(apply(I_j,1,sum))
    }
    doc2 <- doc_ids[doc2_index]
    doc_ids <- doc_ids[-doc2_index]
    comps[i,] <- c(doc1, doc2)
    draws <- draws[,-c(doc1_index, doc2_index)]
  }
  return(comps)
}
