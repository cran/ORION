#' 1D data projection
#'
#' Generation of 1-dimensional data mapping based on a selected pair of classes
#'
#' @importFrom e1071 svm
#' @param dataset list containing at least:
#' \describe{
#' \item{data}{Numeric matrix (features x samples)}
#' \item{labs}{Numeric vector of sample labels}
#' \item{name}{(optional): dataset name}
#' \item{oldLabs}{(optional): original categorical sample labels}
#' }
#'
#' @param comb Numeric vector of length 2 specifying the classes for the data projection
#'
#' @return a list with components:
#' \describe{
#' \item{data}{one-row-matrix consisting in the 1D mapped datapoints, columns describe each sample}
#' \item{labs}{Numeric array describing the labels of the samples}
#' \item{oldLabs}{Categorical array of labels}
#' }
#'
#' @export
#'
projectData <- function(dataset=NULL, comb=NULL){

  data <- as.matrix(dataset$data[apply(dataset$data, 1, function(r) !all(is.na(r))),])
  labs  <- as.numeric(as.character(dataset$labs))
  classes <- sort(unique(labs))

  ###### the labels start from 0 and are consecutive
  if(!all(classes == 0:(length(classes)-1))){
    labs <- match(labs,classes)-1
    dataset$labs <- labs
  }

  ###### dataset has already 1 dimension:
  if(ncol(data)==1){
    return(list("data" = t(data),
                "labs" = labs,
                "name" = dataset$name,
                "oldLabs" = dataset$oldLabs)
           )

  }
  keep <- which(labs %in% comb)
  dk <- t(data[, keep, drop = FALSE])
  model <- try(svm(
    x = dk, y=labs[keep],
    type='C-classification', kernel='linear',
    scale=FALSE, cost = 1000), silent = TRUE)

  w <- if (inherits(model, "try-error") || length(model$coefs) == 0){
    rep(NA, nrow(data))
  }else{
    t(model$coefs) %*% model$SV
  }

  mat <- w %*% data
  colnames(mat) <- colnames(data)
  rownames(mat) <- paste("classes.",comb[1],"+",comb[2], sep = "")

  return(list("data"=mat,
              "labs"=dataset$labs,
              "name"=dataset$name,
              "oldLabs"=dataset$oldLabs)
         )
}
