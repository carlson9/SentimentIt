#' Fit STAN Hierarchical Model
#'
#' Fit STAN hierarchical model
#'
#'
#' @param data A csv file or a vector of batch numbers
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy
#' @param chains The number of chains (defalt is 3)
#' @param iter The number of iteration (defalt is 2500)
#' @param seed Set seed (defalt is 1234)
#' 
#' @return fitStanHier 
#'
#' @author David Carlson
#'
#' @seealso \code{\link{fit_stan_hier}}, \code{\link{ckeck_workers}}, \code{\link{stanWrapper}}
#'
#' @rdname fitStanHier
#'
#' @export
fitStanHier <- function(data, hierarchy_data, hierarchy_var, chains=3, iter=2500, seed=1234){

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  if(is.vector(data)){
    data <- readInData(data)
  }

  if(dim(data)[2] != 7){
    stop("data dimension is incorrect")
  }

  if(!(hierarchy_var %in% colnames(hierarchy_data))){
    stop("hierarchy_var should be a name of a column in hierarchy_data")
  }

  y <- data$result[seq(1, dim(data)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data$document_id_old <- data$document_id
  data$document_id <- as.numeric(as.factor(data$document_id))
  g <- data$document_id[seq(1, dim(data)[1], by=2)]
  h <- data$document_id[seq(1, dim(data)[1], by=2) + 1]
  j <- as.numeric(data$worker_id[seq(1, dim(data)[1], by=2)])
  k <- as.numeric(as.factor(as.character(hierarchy_data[,hierarchy_var])))
  #unique(data$worker_id[seq(1,dim(data)[1],by=2)])
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(k))
  ### need to recode ids ###
  #hold.ids <- sort(unique(g))
  #hold.ids.real <- g
  #length(as.factor(g))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  k <- as.numeric(as.factor(k))
  #q <- as.numeric(as.factor(q))

  fit_hier <- stan(file="~/SentimentIt/model_code_hier.stan",
              data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=chains, iter=iter, seed=seed, control=list(max_treedepth=50))
  return(fit_hier)
}

