pkgname <- "sentimentIt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sentimentIt-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sentimentIt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("batchStatus")
### * batchStatus

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: batchStatus
### Title: Check the status of batch(es)
### Aliases: batchStatus

### ** Examples


## Not run: 
##D  
##D batchStatus(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids[1])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("batchStatus", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checkCert")
### * checkCert

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkCert
### Title: Checks if a worker has a certification
### Aliases: checkCert

### ** Examples


## Not run: 
##D  
##D x <- "ab1"
##D y <- c("a204")
##D check <- checkCert(email, password, x, y)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkCert", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checkWorkers")
### * checkWorkers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkWorkers
### Title: Detect outlying workers
### Aliases: checkWorkers

### ** Examples


## Not run: 
##D ban_workers <- checkWorkers(stan_fit = fit, data = output)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkWorkers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("createBatches")
### * createBatches

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: createBatches
### Title: Create new batches
### Aliases: createBatches

### ** Examples

## Not run: 
##D batch_ids <- createBatches(email = 'researcher@school.edu',
##D     password = 'uniquePassword',
##D     task_setting_id = 8, num_batches = 3)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("createBatches", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("createCert")
### * createCert

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: createCert
### Title: Grants certifications to workers
### Aliases: createCert

### ** Examples


## Not run: 
##D  
##D createCert(email = 'researcher@school.edu',
##D     password = 'uniquePassword',
##D     cert= 'bannedmovie_reviews',
##D     workers = ban_workers)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("createCert", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("createTasks")
### * createTasks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: createTasks
### Title: Create new tasks for comparisons
### Aliases: createTasks

### ** Examples


## Not run: 
##D createTasks(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids[1])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("createTasks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fitStan")
### * fitStan

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fitStan
### Title: Fit a Stan model with results from SentimentIt
### Aliases: fitStan

### ** Examples


## Not run: 
##D fit <- fitStan(data = batch_ids)
##D fit <- fitStan(data = output)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fitStan", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeCompsSep")
### * makeCompsSep

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeCompsSep
### Title: Create comparisons and post to SentimentIt
### Aliases: makeCompsSep

### ** Examples


## Not run: 
##D docInfo <- read.table(email, password, "ReviewsWithIds",header=TRUE)
##D makeCompsSep(email = 'researcher@school.edu',
##D     password = 'uniquePassword',
##D     ids = docInfo[,'ids'], number_per = 10,
##D     batch_id = batch_ids,
##D     question = 'Below is text taken from
##D         two movie reviews. Please choose
##D         the text that you think comes from
##D         the most positive review',
##D     pairwise_path = 'Comparisons/first10.Rdata')
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeCompsSep", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readInData")
### * readInData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readInData
### Title: Read in data provided by the workers
### Aliases: readInData

### ** Examples


## Not run: 
##D output <- readInData(email = 'researcher@school.edu',
##D     password = 'uniquePassword', batch_id = batch_ids[1])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readInData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readText")
### * readText

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readText
### Title: Find/create documents and retrieve IDs
### Aliases: readText

### ** Examples


## Not run: 
##D data(reviews)
##D 
##D docInfo <- readText(email = 'researcher@school.edu',
##D     password = 'uniquePassword', read_documents_from = reviews,
##D     write_documents_to = "ReviewsWithIds.csv", index = 'Review')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readText", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("repostExpired")
### * repostExpired

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: repostExpired
### Title: Repost expired tasks from a batch
### Aliases: repostExpired

### ** Examples

## Not run: 
##D repostExpired(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("repostExpired", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("revokeCert")
### * revokeCert

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: revokeCert
### Title: Revoke a certification for workers
### Aliases: revokeCert

### ** Examples


## Not run: 
##D  
##D revokeCert(email = 'researcher@school.edu',
##D     password = 'uniquePassword',
##D     cert = 'snippets', workers = ban_workers)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("revokeCert", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sentimentIt")
### * sentimentIt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sentimentIt
### Title: sentimentIt
### Aliases: sentimentIt sentimentIt-package sentimentIt

### ** Examples


## Not run: 
##D data(reviews)
##D movies <- sentimentIt(email = 'researcher@school.edu',
##D    password = 'uniquePassword',
##D    read_documents_from = reviews,
##D    write_documents_to = 'ReviewsWithIds.csv',
##D    index = 'Review', task_setting_id = 8,
##D    number_per = 10,
##D    question = 'Below is text taken from
##D        two movie reviews. Please
##D        choose the text that you
##D        think comes from the most
##D        positive review',
##D    pairwise_path = 'Comparisons.Rdata',
##D    certone = 'snippets', certtwo = 'bannedmovie_reviews',
##D    timed = FALSE, check_workers_at = c(1,2),
##D    rest_time = 60, rate = 1/3, threshold = 5,
##D    return_stan = TRUE, return_data = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sentimentIt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
