#' Movie reviews example data set
#'
#' An example dataset of Rotten Tomato movie reviews with the number of stars given by the user. This is the first application in the associated paper introducing SentimentIt.
#'
#' @format A data frame with 500 rows and 2 columns:
#' \describe{
#'   \item{Stars}{rating on scale from 1-5 by Mechanical Turk worker}
#'   \item{Review}{The movie review looked over by worker}
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeCompsSep}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
#' @source \url{https://www.rottentomatoes.com}
"reviews"
