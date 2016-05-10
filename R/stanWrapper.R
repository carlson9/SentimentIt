
#' @export
.stanWrapper <- function(data, hierarchy_data=NULL, hierarchy_var=NULL,
                        returnFit=FALSE, cut_point=1, cut_proportion=0.9,
                        n.questions=50, plot_hist=FALSE, file_path=NULL,
                        chains=3, iter=2500, seed=1234, n.cores=3){

  if(is.vector(data)){
    data <- readInData(data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension mismatches")
  }

  # fit fit_stan or fit_stan_hier
  if(is.null(hierarchy_data)==FALSE & is.null(hierarchy_var)==FALSE){
    fit <- fitStanHier(data=data1, hierarchy_data=hierarchy_data,
                       hierarchy_var=hierarchy_var,
                       chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }
  else{
    fit <- fitStan(data=data1, chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }

  outlying <- checkWorkers(stan_fit=fit, data=data1, cut_point=cut_point,
                           cut_proportion=cut_proportion, n.questions=n.questions,
                           plot_hist=plot_hist, file_path=file_path)

  if(!returnFit){
    return(list(outlying_workers=outlying, stan_fit=NULL))
  }
  else{
    return(list(outlying_workers=outlying, stan_fit=fit))
  }
}
