## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(jsonlite)
library(httr)
library(plyr)
library(testthat)
?GET
setwd("/Users/davidflast/Documents/SentimentIt")
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

# set up tests and data in package
devtools::use_testthat()
## This can be run many times as the code is updates
current.code <- as.package("sentimentIt")
load_all(current.code)
document(current.code)

test_file(path="tests/testthat/readInDataTests.R")

# Create data set for readInData
readInDataExample <- readInData(204:208)
devtools::use_data(readInDataExample)

