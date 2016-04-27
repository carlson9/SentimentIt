#' Detect Outlying Workers
#'
#' Detect outlying workers
#'
#'
#' @param stan_fit A stan fit.
#' @param data The data used to fit stan.
#' @param cut_point A cutoff point to classify posterior coefficients. The proportion of posterior coefficients below \code{cut_point} is used to determine outliers. (Default is 1)
#' @param cut_proportion A cutoff proportion of posterior coefficients below \code{cut_point}. If the proportion of posterior coefficients below \code{cut_points} is higher than \code{cut_proportion}, a worker will be considered as an outlier provided that she answers more than 50 questions. (Default is 0.9)
#' @param plot_hist If TRUE, plot the histogram of workers with a rug plot. (Default is FALSE)
#' @param file_path Save the histogram to path and file name specified. (Default is NULL)
#'
#' @return A vector of outlying workers' IDs whose proportion of posterior coefficients below \code{cut_point} is greater than \code{cut_proportion} and who answered more than 50 questions
#'
#' @author David Carlson
#'
#' @seealso \code{\link{batchStatus}}, \code{\link{batchesWrapper}}, \code{\link{createHITSTimed}},\code{\link{checkWorkers}},\code{\link{createBatches}},
#' \code{\link{createCert}},\code{\link{createHITS}}, \code{\link{createHITSBatch}},\code{\link{createPairwise}}, \code{\link{timedWrapper}},
#' \code{\link{extractCoef}},\code{\link{fitStan}},\code{\link{fitStanHier}},\code{\link{givetakeCert}},\code{\link{makeCompsSep}},
#' \code{\link{readInData}}, \code{\link{readText}},\code{\link{repostExpired}},\code{\link{revokeCert}},\code{\link{stanWrapper}}
#'
#' @rdname checkWorkers
#'
#' @export
checkWorkers <- function(stan_fit, data, cut_point=1, cut_proportion=0.9,
                         plot_hist=FALSE, file_path=NULL){

  if(class(stan_fit) != "stanfit"){
    stop("fit should be class stanfit")
  }

  if(dim(data)[2] != 7){
    stop("data dimension mismatches")
  }

  if(!("worker_id" %in% colnames(data))){
    stop("worker_id is not in data")
  }

  if(!is.numeric(cut_point)){
    stop("cut_point should be numeric")
  }

  if(cut_proportion < 0 | cut_proportion > 1){
    stop("cut_proportion should be in the range between 0 to 1")
  }

  bs <- extract(fit)[["b"]]
  bs_proportion <- vapply(1:dim(bs)[2],
                          function(p){length(which(bs[,p] < cut_point))}, 1)/dim(bs)[1]

  workers <- levels(data$worker_id[seq(1, dim(data)[1], by=2)])
  j <- as.numeric(data$worker_id[seq(1, dim(data)[1], by=2)])
  ban_workers <- workers[which(bs_proportion > cut_proportion & table(j) > 50)]

  if(plot_hist){
    plot(hist(bs, main='Histogram of Worker Estimates'))
    rug(bs)
    if(!is.null(file_path)){
      pdf(file_path)
      hist(bs, main='Histogram of Worker Estimates')
      rug(bs)
      dev.off()
    }
  }

  return(ban_workers)
}
