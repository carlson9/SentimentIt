#' Extract Coefficients
#'
#' Extract coefficients from STAN hierarchical model
#'
#'
#' @param fit A stan hierarchical model fit
#' @param hierarchy_data A file that is used to fit a hierarchical model
#' @param document_var A name of the variable in \code{hierarchy_data} that contains document ID number
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that contains a hierarchy's ID number
#'
#' @author David Carlson
#'
#' @seealso \code{\link{fitStanHier}}
#'
#' @rdname extractCoef
#'
#' @export
extractCoef <- function(fit, hierarchy_data, document_var, hierarchy_var){

  if(!(document_var %in% colnames(hierarchy_data))){
    stop("hierarchy_var should be a name of a column in hierarchy_data")
  }

  if(!(hierarchy_var %in% colnames(hierarchy_data))){
    stop("hierarchy_var should be a name of a column in hierarchy_data")
  }

  # store a corresponding to each document ids
  document_id <- as.numeric(as.character(hierarchy_data[,document_var]))
  a <- summary(fit)$summary[grep("a[", rownames(summary(fit)$summary), "mean",
                                 fixed=TRUE)] # document_id
  if(length(document_id)==length(a)){
    hierarchy_data$a <- a
  }

  # store t corresponding to each hierarchy ids (e.g. country)
  hierarchy_id <- as.numeric(as.factor(as.character(hierarchy_data[,hierarchy_var])))
  t <- summary(fit)$summary[grep("t[", rownames(summary(fit)$summary), "mean",
                                 fixed=TRUE)] # hierarchy
  hierarchy_data$t <- NA
  if(length(unique(hierarchy_id))==length(t)){
    for(i in unique(hierarchy_id)){
      hierarchy_data$t[hierarchy_id==i] <- t[i]
    }
  }

  return(hierarchy_data)
}
