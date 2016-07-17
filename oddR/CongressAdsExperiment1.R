rm(list=ls())
source("~/Dropbox/CATText/Rcode/HITapi.R")

#d<-read.table("~/Dropbox/CATText/WiscAds2008/StoryboardText.csv", sep=',')


#readText(pathfrom="~/Dropbox/CATText/WiscAds2008/StoryboardText.csv", pathto="~/Dropbox/CATText/WiscAds2008/StoryboardWithIds", index='text', sep=',', header=TRUE)
docInfo <- read.table("~/Dropbox/CATText/WiscAds2008/StoryboardWithIds", header=TRUE)

question <- "Please read the two advertisement texts below. Your job is to read both and select the ad that is:
    1.) most negative towards the candidate(s) mentioned, or;
    2.) least positive about the candidate(s) mentioned.

Here are a few rules of thumb to guide you:
    1.) Ads that attack a candidate's personal characteristics (e.g., 'Bob is dishonest.') are generally more negative than ads that  attack a candidate's record or job performance (e.g., 'Bob is too liberal.'').
    2.) Ads that attack a specific candidate alone (e.g., 'Bob is unqualified') are generally more negative than ads that contrast two candidates (e.g., 'Bob is unqualified, but Jill is very experienced').
    3.) Ads that attack a named individual (e.g., 'Bob spent his time in Washington working for fat cats') are generally more negative than ads that attack a general group (e.g., 'We need to stop those fat cats in Washington').
    4.) Ads that state a policy position (e.g., 'Bob will find everyone jobs') are generally less positive than ads that praise a candidate as a person (e.g., 'Bob is a leader.').
    5.) If both advertisements attack a candidate, pick whichever of the two advertisements is most negative.
    6.) If both advertisements praise a candidate, pick whichever of the two advertisements is least positive.
    7.) Do not allow your own political opinions to color your decisions.  Your goal is to select the ad that other coders would recognize as the most negative (or least positive)."



## we want to assign first one hundred to batch 170, remaining to 171

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
  require(jsonlite)
  require(httr)
  args <- list(question=question, ids=pairwise[1:100,], batch_id=170)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                 body = args, content_type_json(),
                 encode='json')
  #  browser()
  out <- fromJSON(rawToChar(as.raw(mypost$content)))

  #out
  #batch_id 170
  
  args <- list(question=question, ids=pairwise[101:9420,], batch_id=171)
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
                 body = args, content_type_json(),
                 encode='json')
  #  browser()
  out2 <- fromJSON(rawToChar(as.raw(mypost$content)))
  
  #out2  
  #batch_id 171

#hits<-createHITSSmall(batch_id=170)
#hits<-createHITSSmall(batch_id=171)


#readInBatch(170, path='~/Dropbox/CATText/WiscAds2008/')
#readInBatch(171, path='~/Dropbox/CATText/WiscAds2008/')
#above returning error
#args <- paste('batch_id=', 171, sep='')
#output<- GET(paste('http://sentimentit.herokuapp.com/api/comparisons/download?', args, sep=''))



data1 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch170.csv')
data2 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch171.csv',header=FALSE)
colnames(data2) <- colnames(data1)
output <- rbind(data1,data2)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_code <- '
data {
int N; // number of comparisons
int M; // number of documents
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second itein comparison
int j[N]; // id map for workers
}
parameters {
real a[M];
real<lower=0> b[P];
real<lower=0> sigma;  
}
model {
sigma~normal(0,3);
for(p in 1:P){
b[p] ~ normal(0,sigma);
}
for(m in 1:M){
a[m] ~ normal(0,1);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'





data <- output
y <- data$result[seq(1,dim(data)[1],by=2)]
z <- y
z[z==0] <- -1
data$document_id_old <- data$document_id
data$document_id <- as.numeric(as.factor(output$document_id))
g <- data$document_id[seq(1,dim(data)[1],by=2)]
h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
j <- as.numeric(data$worker_id[seq(1,dim(data)[1],by=2)])
unique(data$worker_id[seq(1,dim(data)[1],by=2)])
M <- length(unique(c(g,h)))
N <- length(y)
P <- length(unique(j))

### need to recode ids ###
hold.ids <- sort(unique(g))
hold.ids.real <- g
length(as.factor(g))
g <- as.numeric(as.factor(g))
h <- as.numeric(as.factor(h))

fit <- stan(model_code=model_code, data=c("y", "g", "h", "N", "M", "P", "j"),
            chains=3, iter=25000, seed=1234)
save(fit, file="~/Dropbox/CATText/WiscAds2008/Fit1.Rdata")

bs <- summary(fit)$summary[936:1039,"mean"]
workers <- levels(data$worker_id[seq(1,dim(data)[1],by=2)])
workers[j]
pdf('~/Dropbox/CATText/WiscAds2008/run1_workers.pdf')
hist(bs, main='Histogram of Worker Estimates')
rug(bs)
dev.off()

plot(fit, pars=paste('a[',1:935,']', sep=''))

nums <- which(bs<1)
sum(j==nums[1])
sum(j==nums[2])
sum(j==nums[3])
sum(j==nums[4])

sum(j==which.max(bs))

workers[c(nums[2], nums[3])]
#revoked


## with squared

model_code_sq <- '
data {
int N; // number of comparisons
int M; // number of documents
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second itein comparison
int j[N]; // id map for workers
}
parameters {
real a[M];
real<lower=0> b[P];
//real<lower=0> sigma;  
}
model {
//sigma~normal(0,3);
for(p in 1:P){
b[p] ~ normal(0,1);
}
for(m in 1:M){
a[m] ~ normal(0,1);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*pow(a[g[n]]-a[h[n]],2)*fabs(a[g[n]]-a[h[n]])/(a[g[n]]-a[h[n]])));
}
}'


fit_sq <- stan(model_code=model_code_sq, data=c("y", "g", "h", "N", "M", "P", "j"),
            chains=3, iter=25000, seed=1234, init='random', init_r=.5)
save(fit_sq, file="~/Dropbox/CATText/WiscAds2008/Fit1_sq.Rdata")


bs_sq <- summary(fit_sq)$summary[936:1039,"mean"]
workers <- levels(data$worker_id[seq(1,dim(data)[1],by=2)])
workers[j]
pdf('~/Dropbox/CATText/WiscAds2008/run1_workers_squared.pdf')
hist(bs_sq, main='Histogram of Worker Estimates with Squared Difference')
rug(bs_sq)
dev.off()

plot(fit_sq, pars=paste('a[',1:935,']', sep=''))



## more HITs

comp_ids_done <- data$comparison_id
comps_in_batch <- 81843:91162
comps_left <- comps_in_batch[!comps_in_batch%in%comp_ids_done]
#somehow missing twenty, maybe just what Matt sent us
save(comps_left, file='~/Dropbox/CATText/WiscAds2008/comps_left.Rdata')

HITs <- createHITSSmall(comps_left[1:1000], HITsetting=10)
#batch 172


# analysis of 170 and 171

data1 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch170.csv')
data2 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch171.csv',header=FALSE)
colnames(data2) <- colnames(data1)
output <- rbind(data1,data2)


data <- output
y <- data$result[seq(1,dim(data)[1],by=2)]
z <- y
z[z==0] <- -1
data$document_id_old <- data$document_id
data$document_id <- as.numeric(as.factor(output$document_id))
g <- data$document_id[seq(1,dim(data)[1],by=2)]
h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
j <- as.numeric(data$worker_id[seq(1,dim(data)[1],by=2)])
#unique(data$worker_id[seq(1,dim(data)[1],by=2)])
M <- length(unique(c(g,h)))
N <- length(y)
P <- length(unique(j))

load('~/Dropbox/CATText/WiscAds2008/Fit1.Rdata')


#docInfo$id refers to original data, docInfo$ids is data$document_id_old, alphas are data$document_id


library(foreign)
orig_data <- read.dta('~/Dropbox/CATText/WiscAds2008/WiscAds2008_GSHData.dta')

library(stringr)

names <- unique(orig_data$creative)
names.split <- strsplit(names,'')
which(!docInfo$header %in% names)

library(plyr)

matches <- laply(1:dim(docInfo)[1], function (x) which(str_detect(as.character(docInfo$header[x]), names))[1])
which(is.na(matches))
length(unique(matches))

matches2 <- matches[!duplicated(matches)&!is.na(matches)]

ad_tone <- laply(1:length(matches2), function(x) orig_data[orig_data$creative==names[matches2][x],'AD_TONE'][1])


docInfo$ad_tone[!is.na(matches)&!duplicated(matches)] <- ad_tone

prp <- laply(1:length(matches2), function(x) orig_data[orig_data$creative==names[matches2][x],'CNT_PRP'][1])

docInfo$prp[!is.na(matches)&!duplicated(matches)] <- prp

docInfo[104,] <- docInfo[747,]

docInfo <- docInfo[!duplicated(docInfo$ids),]

alphas <- summary(fit)$summary[1:935,"mean"]

docInfo$alphas <- alphas

docInfo$ad_tone[docInfo$ad_tone==99]<- NA

docInfo$prp[is.na(docInfo$ad_tone)] <- NA

docInfo$prp[docInfo$prp==99] <- 2

docInfo$prp[docInfo$prp==4] <- 2

tone <- ifelse(docInfo$ad_tone == 1, 3,
               ifelse(docInfo$ad_tone == 2, 1,
                      ifelse(docInfo$ad_tone==3,5,NA)))

tone <- tone + docInfo$prp - 2

docInfo$tone <- tone
save(docInfo, file='~/Dropbox/CATText/WiscAds2008/docInfo.Rdata')

cor(na.omit(tone), alphas[!is.na(tone)])





## more HITs
## going to use the same set-up, but get every document compared 20 times
docInfo <- read.table("~/Dropbox/CATText/WiscAds2008/StoryboardWithIds", header=TRUE)

question <- "Please read the two advertisement texts below. Your job is to read both and select the ad that is:
1.) most negative towards the candidate(s) mentioned, or;
2.) least positive about the candidate(s) mentioned.

Here are a few rules of thumb to guide you:
1.) Ads that attack a candidate's personal characteristics (e.g., 'Bob is dishonest.') are generally more negative than ads that  attack a candidate's record or job performance (e.g., 'Bob is too liberal.'').
2.) Ads that attack a specific candidate alone (e.g., 'Bob is unqualified') are generally more negative than ads that contrast two candidates (e.g., 'Bob is unqualified, but Jill is very experienced').
3.) Ads that attack a named individual (e.g., 'Bob spent his time in Washington working for fat cats') are generally more negative than ads that attack a general group (e.g., 'We need to stop those fat cats in Washington').
4.) Ads that state a policy position (e.g., 'Bob will find everyone jobs') are generally less positive than ads that praise a candidate as a person (e.g., 'Bob is a leader.').
5.) If both advertisements attack a candidate, pick whichever of the two advertisements is most negative.
6.) If both advertisements praise a candidate, pick whichever of the two advertisements is least positive.
7.) Do not allow your own political opinions to color your decisions.  Your goal is to select the ad that other coders would recognize as the most negative (or least positive)."




#ids <- docInfo[,'ids']
#number_per <- 20
#documents <- as.numeric(ids)

## here we'll make documents a vector containing doc_ids repreated as many times as necessary (i.e. if compared 10 times, but it in 5 times for 10 more comps)
## just going to get rid of repeats in pairwise


require(plyr)
# This code sets up the random pairwise comparisons
docs <- unique(docInfo$ids)
documents <- rep(docs, pmax((20-table(output$document_id)+1)%/%2,0))

pairwise<-cbind(documents, sample(documents,length(documents),replace=FALSE))
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

save(pairwise, file='~/Dropbox/CATText/WiscAds2008/second_comps.Rdata')

#batch 
require(jsonlite)
require(httr)
args <- list(question=question, ids=pairwise[1:1000,], batch_id=174)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

#out
#batch_id 174


#hits<-createHITSSmall(batch_id=174)

args <- list(question=question, ids=pairwise[1001:2000,], batch_id=175)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[2001:3000,], batch_id=176)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

args <- list(question=question, ids=pairwise[3001:3592,], batch_id=177)
args <- toJSON(args, auto_unbox=TRUE)
mypost <- POST('http://sentimentit.herokuapp.com/api/comparisons/create.json',
               body = args, content_type_json(),
               encode='json')
#  browser()
out <- fromJSON(rawToChar(as.raw(mypost$content)))

#hits<-createHITSSmall(batch_id=175)

#hits<-createHITSSmall(batch_id=176)

#hits<-createHITSSmall(batch_id=177)

## final analysis ##


load('~/Dropbox/CATText/WiscAds2008/docInfo.Rdata')

#170, 171, 174-177


data1 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch170.csv')
data2 <- read.csv('~/Dropbox/CATText/WiscAds2008/batch171.csv',header=FALSE)
colnames(data2) <- colnames(data1)
output <- rbind(data1,data2)
for(i in 174:177){
  readInBatch(i,'~/Dropbox/CATText/WiscAds2008/')
  output <- rbind(output, read.csv(paste0('~/Dropbox/CATText/WiscAds2008/batch',i,'.csv')))
}


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_code <- '
data {
int N; // number of comparisons
int M; // number of documents
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second itein comparison
int j[N]; // id map for workers
}
parameters {
real a[M];
real<lower=0> b[P];
real<lower=0> sigma;  
}
model {
sigma~normal(0,3);
for(p in 1:P){
b[p] ~ normal(0,sigma);
}
for(m in 1:M){
a[m] ~ normal(0,1);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'





data <- output
y <- data$result[seq(1,dim(data)[1],by=2)]
z <- y
z[z==0] <- -1
data$document_id_old <- data$document_id
data$document_id <- as.numeric(as.factor(output$document_id))
g <- data$document_id[seq(1,dim(data)[1],by=2)]
h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
j <- as.numeric(data$worker_id[seq(1,dim(data)[1],by=2)])
unique(data$worker_id[seq(1,dim(data)[1],by=2)])
M <- length(unique(c(g,h)))
N <- length(y)
P <- length(unique(j))

### need to recode ids ###
hold.ids <- sort(unique(g))
hold.ids.real <- g
length(as.factor(g))
g <- as.numeric(as.factor(g))
h <- as.numeric(as.factor(h))

fit_total <- stan(model_code=model_code, data=c("y", "g", "h", "N", "M", "P", "j"),
            chains=3, iter=25000, seed=1234)
save(fit_total, file="~/Dropbox/CATText/WiscAds2008/fit_total.Rdata")


bs <- summary(fit_total)$summary[936:1058,"mean"]
workers <- levels(data$worker_id[seq(1,dim(data)[1],by=2)])
workers[j]
pdf('~/Dropbox/CATText/WiscAds2008/run_total_workers.pdf')
hist(bs, main='Histogram of Worker Estimates')
rug(bs)
dev.off()

alphas <- summary(fit_total)$summary[1:935,"mean"]

cor(na.omit(docInfo$tone), alphas[!is.na(docInfo$tone)])

docInfo$alphas <- alphas

save(docInfo, file='~/Dropbox/CATText/WiscAds2008/docInfo.Rdata')

pdf('~/Dropbox/CATText/WiscAds2008/estimate_boxplot.pdf')
boxplot(docInfo$alphas~docInfo$tone, xlab='Human Coded Tone', ylab='Point Estimates', main='Estimate Box Plot on Human Coding')
dev.off()

load('~/Dropbox/CATText/WiscAds2008/docInfo.Rdata')


low <- na.omit(docInfo[docInfo$tone==1,])
#coded as more negative, human coders coded 1 (highest to lowest)
odd_text <- cbind(1, low$alphas[head(order(low$alphas,decreasing=TRUE))],as.character(low$text[head(order(low$alphas,decreasing=TRUE))]))


mid <- na.omit(docInfo[docInfo$tone==3,])
#coded as more negative, human coders coded 3 (highest to lowest)
odd_text <- rbind(odd_text, cbind(3, mid$alphas[head(order(mid$alphas,decreasing=TRUE))],as.character(mid$text[head(order(mid$alphas,decreasing=TRUE))])))
#coded as more positive, human coders coded 3 (lowest to highest)
odd_text <- rbind(odd_text, cbind(3, mid$alphas[head(order(mid$alphas,decreasing=FALSE))],as.character(mid$text[head(order(mid$alphas,decreasing=FALSE))])))


high <- na.omit(docInfo[docInfo$tone==5,])
#coded as more positive, human coders coded 5 (lowest to highest)
odd_text <- rbind(odd_text, cbind(5,high$alphas[head(order(low$alphas,decreasing=FALSE))], as.character(high$text[head(order(low$alphas,decreasing=FALSE))])))

#sink('~/Dropbox/CATText/WiscAds2008/odd_text.txt')
#odd_text
#sink(NULL)

write.table(odd_text, '~/Dropbox/CATText/WiscAds2008/odd_text.csv', sep=',')


#load('~/Dropbox/CATText/WiscAds2008/docInfo.Rdata')


pdf(width=7, height=4, file='~/Dropbox/CATText/WiscAds2008/estimate_points.pdf')
par(mfrow=c(1,1), mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
col1 <- rgb(0, 0, .8, alpha=.3)
plot(jitter(docInfo$tone), docInfo$alphas, pch=19, col=col1, xlab="Human codes", ylab="SentimentIt negativity rating")
abline(lm(docInfo$alphas~docInfo$tone), col="gray80", lwd=2)
dev.off()

tones <- docInfo$ad_tone
tones <- ifelse(tones==1,2,ifelse(tones==2, 1, 3))
pdf(width=7, height=4, file='~/Dropbox/CATText/WiscAds2008/estimate_points3.pdf')
par(mfrow=c(1,1), mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=.9, cex.axis=.8)
col1 <- rgb(0, 0, .8, alpha=.3)
plot(jitter(tones), docInfo$alphas, pch=19, col=col1, xlab="Human codes", ylab="SentimentIt negativity rating",
     xaxt='n')
axis(1, at = c(1,2,3))
abline(lm(docInfo$alphas~tones), col="gray80", lwd=2)
dev.off()

cor(tones[!is.na(tones)], docInfo$alphas[!is.na(tones)])
#.8448

cor(na.omit(docInfo$tone), docInfo$alphas[!is.na(docInfo$tone)])
#.854
