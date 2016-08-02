## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(jsonlite)
library(httr)
library(plyr)
library(testthat)

# Davidflasterstein path
setwd("~/SentimentIt")
# This will need to be changed to match your directory of where the package is.
## This is run once when the package strcuture is first created


## At this point put the *.R files into the correct directories and edit the DESCRIPTION file

## Let's look through the R directory in this order:

# sentimentIt-pckg.R
# createHITSSmall.R
# createPairwise.R
# makeComps_sep.R
# readInData.R
# readText.R

# Now the NAMESPACE


## This can be run many times as the code is updates
current.code <- as.package("SentimentIt")
load_all(current.code)
document(current.code)
check(current.code)
build(current.code)


