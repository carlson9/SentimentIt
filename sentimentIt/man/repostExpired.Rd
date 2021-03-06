% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repostExpired.R
\name{repostExpired}
\alias{repostExpired}
\title{Repost expired tasks from a batch}
\usage{
repostExpired(email, password, batch_id)
}
\arguments{
\item{email}{The researcher's email used for SentimentIt registration}

\item{password}{The researcher's password used for SentimentIt}

\item{batch_id}{ID of batch to check}
}
\value{
out reposted count
}
\description{
Repost expired tasks from a batch
}
\details{
Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
have run the readText() function to upload your data objects to the SentimentIt server. You should have also
run the createBatches() function and saved the batch ID numbers in your R workspace. You should have also
run the makeComps() function to save a portion of your total comparisons under each batch. You should have
also run the createTasks() function to send batches of paired data objects to Mechanical Turk for comparison.
It is advisable to use the batchStatus() function to see how many tasks are incomplete before reposting
the incomplete tasks to Mechanical Turk.

When batches of comparisons are submitted to Mechanical Turk, some comparisons may not be completed before the 
batch expires. These incomplete tasks can be reposted to Mechanical Turk using the `repostExpired()` function.

Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
}
\examples{
\dontrun{
repostExpired(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids)
}
}
\seealso{
\code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
}
\author{
David Carlson
}
