% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authenticate.R
\name{authenticate}
\alias{authenticate}
\title{Authenticate researcher and retrieve token}
\usage{
authenticate(email, password)
}
\arguments{
\item{email}{The email used by the researcher to register with SentimentIt.}

\item{password}{The password associated with the account.}
}
\value{
auth_token Character for the authorization token, which expires after the researcher signs out.
}
\description{
Authenticates researcher and returns authorization token. Primarily used internally but can be used if the researcher wishes to use the API directly.
}
\seealso{
\code{\link{sentimentIt}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
}
\author{
David Carlson
}
