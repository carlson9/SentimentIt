#' Read in Data Example Dataset
#'
#' An example dataset produced by readInData. This includes batches 204-208
#'
#' @format A data frame with 9338 rows and 7 variables:
#' \describe{
#'   \item{batch_id}{the batch number}
#'   \item{comparison_id}{id number of the comparison being made}
#'   \item{document_id}{id number of the document}
#'   \item{result}{result of document comparison}
#'   \item{task_id}{id of the task}
#'   \item{worker_id}{id of the worker who did the comparison}
#'   \item{completed_at}{time comaparison was completed}
#' }
#' @seealso \code{\link{createTasksTimed}}, \code{\link{batchesWrapper}}, \code{\link{checkCert}},
#' \code{\link{checkWorkers}},\code{\link{createBatches}},\code{\link{createCert}},\code{\link{createTasks}}, 
#' \code{\link{createPairwise}}, \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},
#' \code{\link{givetakeCert}},\code{\link{makeCompsSep}},\code{\link{readInData}}, \code{\link{readText}},
#' \code{\link{repostExpired}},\code{\link{revokeCert}}, \code{\link{sentimentIt}}, \code{\link{batchStatus}},
#' \code{\link{extractCoef}}
#' @author Jacob M. Montgomery
#' @source
#'
#' readInData(204:208)
#' @rdname readInDataExample
