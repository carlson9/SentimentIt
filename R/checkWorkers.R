#' Detect Outlying Workers
#'
#' Detect outlying workers
#'
#'
#' @param fit A stan fit
#' @param data The data used to fit stan
#' @param plot If TRUE, create a histogram with a rug plot (defalt is FALSE)
#' @param file Save the histogram to path and file name specified (defalt is NULL)
#'
#' @return Outlying workers' IDs who have beta coefficients less than 1 and answered more than 100
#'
#' @author David Carlson
#'
#' @seealso \code{\link{fitStan}}, \code{\link{fitStanHier}}, \code{\link{stanWrapper}}
#'
#' @rdname checkWorkers
#'
#' @export
checkWorkers <- function(fit, data, plot=FALSE, file=NULL){

  if(!("worker_id" %in% colnames(data))){
    stop("worker_id is not in data")
  }

  bs <- summary(fit)$summary[grep("b", rownames(summary(fit)$summary), "mean")]
  workers <- levels(data$worker_id[seq(1, dim(data)[1], by=2)])
  j <- as.numeric(workers)
  ban_workers <- workers[which(bs < 1 & table(j) > 100)]

  if(plot){
    plot(hist(bs, main='Histogram of Worker Estimates'))
    rug(bs)
    if(!is.null(file)){
      pdf(file)
      hist(bs, main='Histogram of Worker Estimates')
      rug(bs)
      dev.off()
    }
  }

  return(ban_workers)
}
