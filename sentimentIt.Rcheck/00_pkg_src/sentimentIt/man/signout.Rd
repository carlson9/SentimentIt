% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signout.R
\name{signout}
\alias{signout}
\title{Signs the researcher out of the session}
\usage{
signout(email, password)
}
\arguments{
\item{email}{The email used by the researcher to register with SentimentIt.}

\item{password}{The password associated with the account.}
}
\value{
success TRUE if sign out succeeded. Function will be called recursively until success.
}
\description{
This function will revoke the authorization token and sign the researcher out of the session. However, if the certification surveys remain active, if someone takes the survey the researcher will be signed back in.
}
\seealso{
\code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}}
}
\author{
David Carlson
}
