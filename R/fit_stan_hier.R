#' Fit STAN Hierarchical Model
#'
#' Fit STAN hierarchical model
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param hier_data The data used for a hierarchy. One column should be document id and the other column should be a group used as a hierarchy
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#'
#' @author David Carlson
#'
#' @seealso \code{\link{fit_stan_hier}}, \code{\link{ckeck_workers}}, \code{\link{stanWrapper}}
#'
#' @rdname fit_stan_hier
#'
#' @export
fit_stan_hier <- function(data, hier_data, chains, iter, seed){

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  if(is.vector(data)==TRUE){
    data <- readInData(data)
  }

  if(dim(data)[2] != 7){
    stop("data dimension is incorrect")
  }

  if(dim(hier_data)[2] != 2){
    stop("hier_data dimension is incorrect")
  }

  data <- merge(data, hier_data, by.x="document_id", by.y="ids")
  colnames(data)[dim(data)[2]] <- "hierarchy"

  y <- data$result[seq(1, dim(data)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data$document_id_old <- data$document_id
  data$document_id <- as.numeric(as.factor(data$document_id))
  data$hierarchy_id_old <- data$hierarchy
  data$hierarchy_id <- as.numeric(as.factor(data$hierarchy_id_old))
  g <- data$document_id[seq(1, dim(data)[1], by=2)]
  h <- data$document_id[seq(1, dim(data)[1], by=2) + 1]
  k <- data$hierarchy_id[seq(1, dim(data)[1], by=2)]
  j <- as.numeric(data$worker_id[seq(1, dim(data)[1], by=2)])
  #unique(data$worker_id[seq(1,dim(data)[1],by=2)])
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(c(q, k)))
  ### need to recode ids ###
  #hold.ids <- sort(unique(g))
  #hold.ids.real <- g
  #length(as.factor(g))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  k <- as.numeric(as.factor(k))
  q <- as.numeric(as.factor(q))

  fit_hier <- stan(file="~/SentimentIt/model_code_hier.stan",
              data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=chains, iter=iter, seed=seed, control=list(max_treedepth=50))
  return(fit_hier)
}
