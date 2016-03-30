rm(list=ls())
source("~/Dropbox/CATText/Rcode/HITapi.R")


#d<-read.table("~/Dropbox/CATText/HR data/State_Dept_Torture_1999_2.csv", sep=',', header=TRUE)


#readText(pathfrom="~/Dropbox/CATText/HR data/State_Dept_Torture_1999_2.csv", pathto="~/Dropbox/CATText/HR data/State_Dept_Torture_1999WithIds", index=4, sep=',', header=TRUE)
docInfo <- read.table("~/Dropbox/CATText/HR data/State_Dept_Torture_1999WithIds", header=TRUE)

docInfo <- docInfo[!duplicated(docInfo$ids),]

question <- "Which of the two statements show more significant levels of torture? Torture is more significant if it there is evidence it is more frequent, more severe, unpunished, or systematic. Legal punishments and maltreatment of prisoners that is not used to intimidate or extract confessions are not considered to be torture."



## we want to assign by 1000s

ids <- docInfo[,'ids']
number_per <- 20
  documents <- as.numeric(ids)
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
  save(pairwise,file='~/Dropbox/CATText/HR data/pairwise.Rdata')
  require(jsonlite)
  require(httr)
  args <- list(question=question, ids=pairwise[1:1000,], batch_id=178)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                 body = args, content_type_json(),
                 encode='json')
  #  browser()
  out <- fromJSON(rawToChar(as.raw(mypost$content)))
#load('~/Dropbox/CATText/HR data/pairwise.Rdata')
args <- list(question=question, ids=pairwise[1001:2000,], batch_id=179)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[2001:3000,], batch_id=180)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[3001:4000,], batch_id=181)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[4001:5000,], batch_id=182)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[5001:6000,], batch_id=183)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[6001:7000,], batch_id=184)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[7001:8000,], batch_id=185)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[8001:9000,], batch_id=186)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[9001:10000,], batch_id=187)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[10001:11000,], batch_id=188)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[11001:12000,], batch_id=189)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[12001:13000,], batch_id=190)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[13001:14000,], batch_id=191)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[14001:15000,], batch_id=192)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))


args <- list(question=question, ids=pairwise[15001:16050,], batch_id=193)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))



#hits<-createHITSSmall(batch_id=178)
#hits<-createHITSSmall(batch_id=179)
#hits<-createHITSSmall(batch_id=180)
#hits<-createHITSSmall(batch_id=181)
#hits<-createHITSSmall(batch_id=182)
#hits<-createHITSSmall(batch_id=183)
#hits<-createHITSSmall(batch_id=184)
#hits<-createHITSSmall(batch_id=185)
#hits<-createHITSSmall(batch_id=186)
#hits<-createHITSSmall(batch_id=187)
#hits<-createHITSSmall(batch_id=188)
#hits<-createHITSSmall(batch_id=189)
#hits<-createHITSSmall(batch_id=190)
#hits<-createHITSSmall(batch_id=191)
#hits<-createHITSSmall(batch_id=192)
#hits<-createHITSSmall(batch_id=193)

output <- NULL
for(i in 178:193){
  readInBatch(i, path='~/Dropbox/CATText/HR data/')
  output <- rbind(output, read.csv(paste0('~/Dropbox/CATText/HR data/batch',i,'.csv')))
}

g <- output$document_id[seq(1,dim(output)[1],by=2)]
h <- output$document_id[seq(1,dim(output)[1],by=2)+1]
docs_wanted <- apply(pairwise, 1, paste, collapse=' ')

needed_docs <- which(!(docs_wanted %in% apply(cbind(g,h),1,paste, collapse=' ') | docs_wanted %in% apply(cbind(h,g),1,paste, collapse=' ')))



args <- list(question=question, ids=pairwise[needed_docs,], batch_id=194)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

#hits<-createHITSSmall(batch_id=194)

readInBatch(194, path='~/Dropbox/CATText/HR data/')
output <- rbind(output, read.csv('~/Dropbox/CATText/HR data/batch194.csv'))

g <- output$document_id[seq(1,dim(output)[1],by=2)]
h <- output$document_id[seq(1,dim(output)[1],by=2)+1]
docs_wanted <- apply(pairwise, 1, paste, collapse=' ')

needed_docs <- which(!(docs_wanted %in% apply(cbind(g,h),1,paste, collapse=' ') | docs_wanted %in% apply(cbind(h,g),1,paste, collapse=' ')))
save(output, file='~/Dropbox/CATText/HR data/all_batches.Rdata')

#load('~/Dropbox/CATText/HR data/all_batches.Rdata')






######################
#### Analysis ########
######################




docInfo2 <- read.table('~/Dropbox/CATText/HR data/State_Dept_Torture_1999WithIds',  sep='\t', header=TRUE, quote='\"')
docInfo2 <- docInfo2[-1,]
load('~/Dropbox/CATText/HR data/all_batches.Rdata')
dups <- docInfo2[which(duplicated(docInfo2$ids)),'ids']
docInfo2$ids <- as.numeric(as.character(docInfo2$ids))
docInfo2[which(duplicated(docInfo2$ids)),'ids'] <- 9500:9546
#output[,'document_id']%in%dups
i<-1
mapper<-9500:9546
for(dup in dups){
  reps <- which(output$document_id==dup)
  for(rep in reps){
    if(rep%%2!=0){
      tobind <- output[rep:(rep+1),]
      tobind[1,'document_id'] <- mapper[i]
    }else{
      tobind <- output[(rep-1):rep,]
      tobind[2,'document_id'] <- mapper[i]
    }
    output <- rbind(output, tobind)
  }
  i<-i+1
}
save(output, file='~/Dropbox/CATText/HR data/all_batches_with_dups.Rdata')
docInfo2 <- docInfo2[order(docInfo2$ids),]
countries <- docInfo2$V1

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



data <- output
y <- data$result[seq(1,dim(data)[1],by=2)]
#z <- y
#z[z==0] <- -1
data$document_id_old <- data$document_id
data$document_id <- as.numeric(as.factor(data$document_id))
#data$country_id_old <- data$countries
#data$country_id <- as.numeric(data$country_id_old)
g <- data$document_id[seq(1,dim(data)[1],by=2)]
h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
countries <- as.numeric(as.factor(data$countries))
j <- as.numeric(data$worker_id[seq(1,dim(data)[1],by=2)])
#unique(data$worker_id[seq(1,dim(data)[1],by=2)])
M <- length(unique(c(g,h)))
N <- length(y)
P <- length(unique(j))
D <- length(unique(c(q,k)))
### need to recode ids ###
#hold.ids <- sort(unique(g))
#hold.ids.real <- g
#length(as.factor(g))
#g <- as.numeric(as.factor(g))
#h <- as.numeric(as.factor(h))
#k <- as.numeric(as.factor(k))
#q <- as.numeric(as.factor(q))


## get mapper

countries <- as.numeric(as.factor(as.character(docInfo2$V1)))

k <- countries
save(docInfo2, k, file='~/Dropbox/CATText/HR data/docInfo2.Rdata')

model_code7 <- '
data {
int N; // number of comparisons
int M; // number of paragraphs
int D; // number of documents (countries)
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second item in comparison
int j[N]; // id map for workers
int k[M]; // id map for documents (countries) relating to documents
}
parameters {
real a[M]; // paragraphs
real t[D]; // documents (countries)
real<lower=0> b[P];
real<lower=0> sigmac[D];
//real<lower=0> sigma;  
}
model {
//sigma~normal(0,3);
//t ~ normal(0, 1);
for(p in 1:P){
b[p] ~ normal(0,1);
}
for(d in 1:D){
t[d] ~ normal(0,1);
sigmac[d] ~ normal(0,.5);
}
for(m in 1:M){
a[m] ~ normal(t[k[m]],sigmac[k[m]]);
}
for(n in 1:N) {
//a[g[n]] ~ normal(t[k[n]], sigmac[k[n]]);
//a[h[n]] ~ normal(t[q[n]], sigmac[q[n]]);
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'


fit7 <- stan(model_code=model_code7, data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
             chains=3, iter=2500, seed=1234, control=list(max_treedepth=50))
#, control=list(adapt_delta = 0.99, stepsize=.5))
save(fit7, file="~/Dropbox/CATText/HR data/Fit7.Rdata")

#docInfo2 <- docInfo2[order(docInfo2$ids),]
docInfo2$alphas <- summary(fit7)$summary[1:1652,'mean']
#c <- k[!duplicated(k)]
#reps <- table(k)[(c[order(c)])]
docInfo2$doc_est <- summary(fit7)$summary[1653:1834,'mean'][k]
docInfo2$doc_sd <- summary(fit7)$summary[1892:2071,'mean'][k]
save(docInfo2, file='~/Dropbox/CATText/HR data/docInfo2.Rdata')

metadata <- read.csv('~/Dropbox/CATText/HR data/reports_metadata.csv')
metadata <- metadata[metadata$year.0==1999,]
needed <- metadata[,c('country_iso3c', 'hathaway', 'state')]
colnames(docInfo2)[2] <- 'country_iso3c'
hold <- needed[needed$country_iso3c%in%docInfo2$country_iso3,]
hold <- hold[!duplicated(hold),]
#hold2 <- merge(docInfo, needed, by='country_iso3c', all=FALSE)
docInfo2$hathaway <- docInfo2$state <- NA
for(country in hold$country_iso3c){
  docInfo2$hathaway[docInfo2$country_iso3c==country] <- hold[hold$country_iso3c==country,'hathaway']
  docInfo2$state[docInfo2$country_iso3c==country] <- hold[hold$country_iso3c==country,'state']
}
save(docInfo2, file='~/Dropbox/CATText/HR data/docInfo2.Rdata')
mean_alpha<-ts<-max_alphas<-sum_alphas<-numeric()
hathaway <- numeric()
state <- numeric()

for(doc in unique(docInfo2$country_iso3c)){
  mean_alpha <- c(mean_alpha, mean(docInfo2$alphas[docInfo2$country_iso3c==doc]))
  ts <- c(ts, docInfo2$doc_est[docInfo2$country_iso3==doc][1])
  max_alphas <- c(max_alphas, max(docInfo2$alphas[docInfo2$country_iso3c==doc]))
  sum_alphas <- c(sum_alphas, sum(docInfo2$alphas[docInfo2$country_iso3c==doc]))  
  hathaway <- c(hathaway, docInfo2$hathaway[docInfo2$country_iso3c==doc][1])
  state <- c(state, docInfo2$state[docInfo2$country_iso3c==doc][1])  
}
whichNA<-which(is.na(docInfo2$hathaway))
cor(docInfo2$alphas[-whichNA],docInfo2$hathaway[-whichNA])

whichNA <- which(is.na(hathaway))
cor(mean_alpha[-whichNA],hathaway[-whichNA])
cor(sum_alphas[-whichNA],hathaway[-whichNA])
cor(max_alphas[-whichNA],hathaway[-whichNA])
cor(ts[-whichNA],hathaway[-whichNA])

whichNA <- which(is.na(state))
cor(mean_alpha[-whichNA],state[-whichNA])
cor(sum_alphas[-whichNA],state[-whichNA])
cor(max_alphas[-whichNA],state[-whichNA])
cor(ts[-whichNA],state[-whichNA])




save(docInfo2, file='~/Dropbox/CATText/HR data/docInfo2.Rdata')

pdf('~/Dropbox/CATText/HR data/document_plot.pdf', height=10)
plot(unique(docInfo$doc_est)[order(unique(docInfo$doc_est))],1:180, ylab='', xlab='Document Estimate',
     yaxt='n',cex=.5,pch=18)
axis(2, at=1:180, tick=FALSE,
     labels = unique(docInfo$country_iso3c)[order(unique(docInfo$doc_est))],
     las=1, cex.axis=.3)
dev.off()

load('~/Dropbox/CATText/HR data/docInfo2.Rdata')


hat_ord <- order(docInfo2$hathaway)

hold <- docInfo2[hat_ord,]
lines_to_draw <- which(!duplicated(hold$hathaway))-1
#lines_to_draw[1]<-1
pdf('~/Dropbox/CATText/HR data/alphas_plot.pdf')#, height=20)
#par(mfrow=c(3,2))
for(cut in 1:5){
  hold1 <- hold[(lines_to_draw[cut]+1):(lines_to_draw[cut+1]),]
  plot(unique(hold1$doc_est)[order(unique(hold1$doc_est))],1:length(unique(hold1$doc_est)), ylab='', xlab='Document Estimate',
       yaxt='n',cex=.5,pch=18, type='n', xlim=c(-4,4), main=paste('Coded as',cut))
  
  i <- 1
  for(country in unique(hold1$country_iso3c)){
    points(hold1$alphas[hold1$country_iso3c==country],rep(i, sum(hold1$country_iso3c==country)), pch=18, cex=1)
    points(hold1$doc_est[hold1$country_iso3c==country][1],i, pch=20, col='red')
    abline(h=i, cex=.3, col='grey30')
    i <- i+1
  }
  abline(v=0, lty=3)
  axis(2, at=1:(i-1), tick=FALSE,
       labels = unique(hold1$country_iso3c),
       las=1, cex.axis=1)
  
}

hold1 <- hold[(lines_to_draw[6]+1):1652,]
plot(unique(hold1$doc_est)[order(unique(hold1$doc_est))],1:length(unique(hold1$doc_est)), ylab='', xlab='Document Estimate',
     yaxt='n',cex=.5,pch=18, type='n', xlim=c(-4,4), main='Coded as NA')

i <- 1
for(country in unique(hold1$country_iso3c)){
  points(hold1$alphas[hold1$country_iso3c==country],rep(i, sum(hold1$country_iso3c==country)), pch=18, cex=1)
  points(hold1$doc_est[hold1$country_iso3c==country][1],i, pch=20, col='red')
  abline(h=i, cex=.3, col='grey30')
  i <- i+1
}
abline(v=0, lty=3)

axis(2, at=1:(i-1), tick=FALSE,
     labels = unique(hold1$country_iso3c),
     las=1, cex.axis=1)

dev.off()






#### new data?

fariss <- read.csv("~/Dropbox/CATText/HR data/HumanRightsProtectionScores_v2.04.csv")
fariss<-fariss[fariss$YEAR==1999,]

d<-read.table("~/Dropbox/CATText/HR data/State_Dept_Torture_1999_2.csv", sep=',', header=TRUE)
data <- d[!duplicated(d$V1),c('V1','V2')]
order(data$V2)

write.csv(data.frame(ccode = data$V1, country = data$V2, hathaway = NA), "~/Dropbox/CATText/HR data/hathaway.csv")

hath <- read.csv("~/Dropbox/CATText/HR data/hathaway.csv")

docInfo <- docInfo2[,-10]
colnames(docInfo)[2] <- 'ccode'
docs <- merge(docInfo, hath, by='ccode')


save(docs, file='~/Dropbox/CATText/HR data/docs.Rdata')
mean_alpha<-ts<-max_alphas<-sum_alphas<-numeric()
hathaway <- numeric()
state <- numeric()

for(doc in unique(docs$ccode)){
  mean_alpha <- c(mean_alpha, mean(docs$alphas[docs$ccode==doc]))
  ts <- c(ts, docs$doc_est[docs$ccode==doc][1])
  max_alphas <- c(max_alphas, max(docs$alphas[docs$ccode==doc]))
  sum_alphas <- c(sum_alphas, sum(docs$alphas[docs$ccode==doc]))  
  hathaway <- c(hathaway, docs$hathaway[docs$ccode==doc][1])
  state <- c(state, docs$state[docs$ccode==doc][1])  
}
whichNA<-which(is.na(docs$hathaway))
cor(docs$alphas[-whichNA],docs$hathaway[-whichNA])

whichNA <- which(is.na(hathaway))
cor(mean_alpha[-whichNA],hathaway[-whichNA])
cor(sum_alphas[-whichNA],hathaway[-whichNA])
cor(max_alphas[-whichNA],hathaway[-whichNA])
cor(ts[-whichNA],hathaway[-whichNA])

whichNA <- which(is.na(state))
cor(mean_alpha[-whichNA],state[-whichNA])
cor(sum_alphas[-whichNA],state[-whichNA])
cor(max_alphas[-whichNA],state[-whichNA])
cor(ts[-whichNA],state[-whichNA])


hat_ord <- order(docs$hathaway)

hold <- docs[hat_ord,]
lines_to_draw <- which(!duplicated(hold$hathaway))-1
#lines_to_draw[1]<-1
pdf('~/Dropbox/CATText/HR data/alphas_plot.pdf')#, height=20)
#par(mfrow=c(3,2))
par(mar=c(5.1,5.1,4.1,2.1))
for(cut in 1:5){
  hold1 <- hold[(lines_to_draw[cut]+1):(lines_to_draw[cut+1]),]
  plot(unique(hold1$doc_est)[order(unique(hold1$doc_est))],1:length(unique(hold1$doc_est)), ylab='', xlab='Document Estimate',
       yaxt='n',cex=.5,pch=18, type='n', xlim=c(-4,4), main=paste('Coded as',cut))
  
  i <- 1
  for(country in unique(hold1$ccode)){
    points(hold1$alphas[hold1$ccode==country],rep(i, sum(hold1$ccode==country)), pch=18, cex=1)
    points(hold1$doc_est[hold1$ccode==country][1],i, pch=20, col='red')
    abline(h=i, cex=.3, col='grey30')
    i <- i+1
  }
  abline(v=0, lty=3)
  axis(2, at=1:(i-1), tick=FALSE,
       labels = unique(hold1$ccode),
       las=1, cex.axis=1)
  
}

hold1 <- hold[(lines_to_draw[6]+1):1652,]
plot(unique(hold1$doc_est)[order(unique(hold1$doc_est))],1:length(unique(hold1$doc_est)), ylab='', xlab='Document Estimate',
     yaxt='n',cex=.5,pch=18, type='n', xlim=c(-4,4), main='Coded as NA')

i <- 1
for(country in unique(hold1$ccode)){
  points(hold1$alphas[hold1$ccode==country],rep(i, sum(hold1$ccode==country)), pch=18, cex=1)
  points(hold1$doc_est[hold1$ccode==country][1],i, pch=20, col='red')
  abline(h=i, cex=.3, col='grey30')
  i <- i+1
}
abline(v=0, lty=3)

axis(2, at=1:(i-1), tick=FALSE,
     labels = unique(hold1$ccode),
     las=1, cex.axis=1)

dev.off()

## hathaway analysis

library(WDI)
#WDIsearch('GNP')
#WDIsearch('population, total')
#WDIsearch('population growth')
#WDIsearch('trade')
#WDIsearch('aid')
##NAs replaced by 0s in her analysis
#WDIsearch('gdp growth')

#cps variables: international conflict, civil and ethnic war

#polity variables: state failure,
#new regime (durable: 1=5 years or fewer), democracy (0-10 scale)

#lagged DV

wdivars <- WDI(country='all', indicator=c('NY.GNP.PCAP.KD', 'SP.POP.TOTL',
                               'SP.POP.GROW', 'NE.TRD.GNFS.ZS',
                               'DT.ODA.OATL.KD', 'NY.GDP.MKTP.KD.ZG'),
    start=1999, end=1999, extra=TRUE)

wdivars$DT.ODA.OATL.KD[is.na(wdivars$DT.ODA.OATL.KD)] <- 0

#genocide convention

library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_parties_to_the_Genocide_Convention#State_that_has_signed_but_not_ratified'

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")
gen_conv <- html_table(temp[1])[[1]][,c('State','Deposited')]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
gen_conv[,2] <- as.numeric(as.character(substrRight(gen_conv[,2], 4)))
gen_conv$yearsIF <- 1999-gen_conv[,2]

##treaties

treaties <- read.csv('~/Dropbox/CATText/HR data/hathaway.csv')
colnames(wdivars)[10] <- 'ccode'

wdivars[is.na(wdivars$ccode),'ccode'][2] <- 'CPV'

data_to_test <- merge(wdivars, treaties, by = 'ccode', all=FALSE)
#drops TWN, NRU

doc_ests <- docs[,c('ccode', 'doc_est')]
doc_ests <- doc_ests[!duplicated(doc_ests$ccode),]

#doc_ests$ccode[!doc_ests$ccode%in%data_to_test$ccode]

data_to_test <- merge(data_to_test, doc_ests, by='ccode', all=FALSE)

save(data_to_test, file='~/Dropbox/CATText/HR data/data_to_test.Rdata')

cpsdata <- read.csv('~/Dropbox/CATText/HR data/cps.csv')

#cpsdata$COUNTRY[!cpsdata$SCODE%in%data_to_test$ccode]
#going to hand code - polity matches with cpsdata so merge them first
polity <- read.csv('~/Dropbox/CATText/HR data/polity.csv')
polity <- polity[polity$year==1999,]

write.csv(polity,'~/Dropbox/CATText/HR data/polity.csv')


#library(countrycode)
#cpsdata[,10]<-countrycode(cpsdata$CCODE, 'cown', 'iso3c')

#cpsdata[which(is.na(cpsdata[,10])),]

#cpsdata[which(is.na(cpsdata[,10])),10] <- c('ETH', NA, 'SRB')

cpsdata <- cpsdata[,c(2,8,9)]
cpsdata$civ_eth <- apply(cpsdata[,c(2,3)], 1, sum)
cpsdata <- cpsdata[,c(1,4)]
colnames(cpsdata)[1] <- 'ccode'

polity <- polity[,c(4, 9, 13)]
colnames(polity)[1] <- 'ccode'
polcps <- merge(polity, cpsdata, by='ccode')

write.csv(polcps, '~/Dropbox/CATText/HR data/polcps.csv')

#### change data_to_test from wb to cowc
data_to_test$ccode <- countrycode(data_to_test$ccode, 'wb','cowc')
data_to_test[!data_to_test$ccode%in%cpsdata$ccode,
             'ccode'][c(7,9:10,24)] <- 
  c('IVO', 'ETI', 'FJI', 'RUM')


merged_data <- merge(data_to_test, polcps, by='ccode', all=FALSE)
merged_data[,'durable'] <- as.numeric(merged_data$durable>5)

#save(data_to_test, file='~/Dropbox/CATText/HR data/data_to_test.Rdata')
#save(merged_data, file='~/Dropbox/CATText/HR data/merged.Rdata')

#colnames(merged_data)
whichNA <- which(is.na(data_to_test$hathaway))
cor(data_to_test$hathaway[-c(whichNA)], data_to_test$gen_conv[-c(whichNA)])
#.02178
cor(data_to_test$doc_est[-c(whichNA)], data_to_test$gen_conv[-c(whichNA)])
#-.02
cor(data_to_test$hathaway[-c(whichNA)], data_to_test$tort_conv[-c(whichNA)])
#-.0369
cor(data_to_test$doc_est[-c(whichNA)], data_to_test$tort_conv[-c(whichNA)])
#-.0169

mod_h_gen <- lm(hathaway ~ gen_conv + NY.GNP.PCAP.KD + SP.POP.TOTL + SP.POP.GROW + 
     NE.TRD.GNFS.ZS + DT.ODA.OATL.KD + NY.GDP.MKTP.KD.ZG + civ_eth + democ + durable,
   data=merged_data)
mod_d_gen <- lm(doc_est ~ gen_conv + NY.GNP.PCAP.KD + SP.POP.TOTL + SP.POP.GROW + 
                  NE.TRD.GNFS.ZS + DT.ODA.OATL.KD + NY.GDP.MKTP.KD.ZG + civ_eth + democ + durable,
                data=merged_data[!is.na(merged_data$hathaway),])
mod_h_tort <- lm(hathaway ~ tort_conv + NY.GNP.PCAP.KD + SP.POP.TOTL + SP.POP.GROW + 
                  NE.TRD.GNFS.ZS + DT.ODA.OATL.KD + NY.GDP.MKTP.KD.ZG + civ_eth + democ + durable,
                data=merged_data)
mod_d_tort <- lm(doc_est ~ tort_conv + NY.GNP.PCAP.KD + SP.POP.TOTL + SP.POP.GROW + 
                  NE.TRD.GNFS.ZS + DT.ODA.OATL.KD + NY.GDP.MKTP.KD.ZG + civ_eth + democ + durable,
                data=merged_data[!is.na(merged_data$hathaway),])

library(stargazer)
stargazer(mod_h_gen, mod_d_gen,
          mod_h_tort, mod_d_tort, star.cutoffs=NA)


## plots

dochold <- docs[!duplicated(docs$ccode),]
pdf(width=7, height=4, file='~/Dropbox/CATText/HR data/HRValid.pdf')
par(mfrow=c(1,1), mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
col1 <- rgb(0, 0, .8, alpha=.3)
plot(jitter(dochold$hathaway), dochold$doc_est, pch=19, col=col1, xlab="Hathaway codes", ylab="SentimentIt document level estimate of torture")
abline(lm(dochold$doc_est~dochold$hathaway), col="gray80", lwd=2)
dev.off()

set.seed(99)
hold <- docs[!is.na(docs$hathaway),]

#make 5 1s, 8 2s, 10 3s, 8 4s and 8 5s to keep some balance

hold1 <- hold[hold$hathaway==1,]
coun <- sample(unique(hold1$ccode), 5, replace=FALSE)
hold1 <- hold1[hold1$ccode%in%coun,]

hold2 <- hold[hold$hathaway==2,]
coun <- sample(unique(hold2$ccode), 8, replace=FALSE)
hold2 <- hold2[hold2$ccode%in%coun,]

hold3 <- hold[hold$hathaway==3,]
coun <- sample(unique(hold3$ccode), 10, replace=FALSE)
hold3 <- hold3[hold3$ccode%in%coun,]


hold4 <- hold[hold$hathaway==4,]
coun <- sample(unique(hold4$ccode), 8, replace=FALSE)
hold4 <- hold4[hold4$ccode%in%coun,]

hold5 <- hold[hold$hathaway==5,]
coun <- sample(unique(hold5$ccode), 8, replace=FALSE)
hold5 <- hold5[hold5$ccode%in%coun,]

hold <- rbind(hold1, hold2, hold3, hold4, hold5)


lines_to_draw <- which(!duplicated(hold$hathaway))-1
lines_to_draw[6] <- dim(hold)[1]
pdf(file='~/Dropbox/CATText/HR data/HRrandomAlphas.pdf')
par(mar=c(4,3,2,2.1))
plot(unique(hold$doc_est),1:length(unique(hold$doc_est)), ylab='', xlab='Document Estimate',
     yaxt='n',cex=.5,pch=18, type='n', xlim=c(-4,4))
num_countries <- c(0,5,13,23,31)
labels.countries <- character()
for(cut in 1:5){
  holdtemp <- hold[(lines_to_draw[cut]+1):(lines_to_draw[cut+1]),]
  holdtemp <- holdtemp[order(holdtemp$doc_est),]
  i <- num_countries[cut]+1
  for(country in unique(holdtemp$ccode)){
    labels.countries <- c(labels.countries, country)
    points(holdtemp$alphas[holdtemp$ccode==country],rep(i, sum(holdtemp$ccode==country)), pch=18, cex=1)
    points(holdtemp$doc_est[holdtemp$ccode==country][1],i, pch=20, col='red')
    abline(h=i, cex=.3, col='grey30', lty=2)
    i <- i+1
  }
}
abline(v=0, lty=3)
abline(h=num_countries[-1]+.5, cex=4)
axis(2, at=1:(i-1), tick=FALSE,
     labels = labels.countries,
     las=1, cex.axis=1)
axis(4, at = c(2.5, 9, 18, 27, 35), labels=1:5, tick=FALSE, las=1)
mtext('Country', side=3, adj=-.11)
mtext('Code', side=3, adj=1.075)
dev.off()


