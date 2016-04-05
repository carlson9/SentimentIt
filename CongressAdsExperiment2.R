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

docInfo2 <- docInfo[!duplicated(docInfo$ids),]
set.seed(123)
docInfo2 <- docInfo2[sample(1:dim(docInfo2)[1], dim(docInfo2)[1]%/%2, replace=FALSE),]

save(docInfo2, file='~/Dropbox/CATText/WiscAds2008/docInfo2.Rdata')

ids <- docInfo2[,'ids']
number_per <- 20

## we need 5 batches - 204:208

makeComps_sep(ids, number_per, batches=204:208, question, path = '~/Dropbox/CATText/WiscAds2008/', name='second')
#hits<-createHITSSmall(batch_id=204)

#####

batchNumber <- 204
require(httr)
require(jsonlite)
#https://sentimentit.com/api/batches/1/download.json 

output<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',batchNumber,'/download.json'))
myurl <- rawToChar(as.raw(output$content))
myurl <- strsplit(myurl,'\"')[[1]][4]

library(RCurl)
x <- getURL(myurl)
output <- read.csv(text = x)


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
            chains=2, iter=5000, seed=1234)
save(fit, file="~/Dropbox/CATText/WiscAds2008/Fit2.1.Rdata")


#######
bs <- summary(fit)$summary[468:482,"mean"]
workers <- levels(data$worker_id[seq(1,dim(data)[1],by=2)])
workers[j]
pdf('~/Dropbox/CATText/WiscAds2008/run2.1_workers.pdf')
hist(bs, main='Histogram of Worker Estimates')
rug(bs)
dev.off()

sum(data$worker_id[seq(1,dim(data)[1],by=2)]==workers[10])
#worker 10 AA500UN82FYES lowest by far at .4 and answered 395 -- banned



#hits<-createHITSSmall(batch_id=205)
#hits<-createHITSSmall(batch_id=206)

myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',205,'/download.json'))
myurl <- rawToChar(as.raw(myurl$content))
myurl <- strsplit(myurl,'\"')[[1]][4]


x <- getURL(myurl)
output <- rbind(output,read.csv(text = x))

myurl<- GET(paste0('https://sentimentit.herokuapp.com/api/batches/',206,'/download.json'))
myurl <- rawToChar(as.raw(myurl$content))
myurl <- strsplit(myurl,'\"')[[1]][4]


x <- getURL(myurl)
output <- rbind(output,read.csv(text = x))



data <- output
y <- data$result[seq(1,dim(data)[1],by=2)]
z <- y
z[z==0] <- -1
data$document_id_old <- data$document_id
data$document_id <- as.numeric(as.factor(output$document_id))
g <- data$document_id[seq(1,dim(data)[1],by=2)]
h <- data$document_id[seq(1,dim(data)[1],by=2)+1]
data$worker_id <- unclass(data$worker_id)
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
            chains=2, iter=5000, seed=1234)
save(fit, file="~/Dropbox/CATText/WiscAds2008/Fit2.2.Rdata")


bs <- summary(fit)$summary[468:482,"mean"]
workers <- levels(data$worker_id[seq(1,dim(data)[1],by=2)])
workers[j]
pdf('~/Dropbox/CATText/WiscAds2008/run2.2_workers.pdf')
hist(bs, main='Histogram of Worker Estimates')
rug(bs)
dev.off()

sum(data$worker_id[seq(1,dim(data)[1],by=2)]==workers[10])
#worker 10 AA500UN82FYES lowest by far at .3 already banned



#hits<-createHITSSmall(batch_id=207)
#hits<-createHITSSmall(batch_id=208)


