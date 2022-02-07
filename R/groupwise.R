#' Coerce to a Groupwise object 
#' 
#' Converts from a Subcascades object to a Groupwise object.
#' 
#' @inheritParams summarySubcascades
#' 
#' @param maxCl 
#' An integer defining the lower bound for the maximal number of classes. Has only to be set if the analyzed dataset has more than 50 classes.
#' 
#' @details 
#' This function re-sorts the Subcascades object in a way that the cascades made up of the same classes are grouped.
#' 
#' @return 
#' A Groupwise object, which comprises a two-leveled list. The first level collects cascades of the same size.
#' The second level contains a list of unique class combinations, labelled as a character string with '-' separating the different classes. 
#' For each unique set of class combinations the corresponding orders and their performance are given as a matrix, 
#' where each row contains a cascade, given as a character string of type '1>2>3', and the columns the sensitivity for the class at the corresponding position.
#' Each matrix is sorted row-wise according to the achieved minimal classwise sensitivites of the cascades (decreasing).
#' 
#' @seealso \code{\link{as.subcascades}}, \code{\link{summary.Groupwise}}, \code{\link{print.Groupwise}}, \code{\link{plot.Groupwise}}, \code{\link{as.edgedataframe}}
#' 
#' @examples
#' 
#' library(TunePareto)
#' data(esl)
#' data = esl$data
#' labels = esl$labels
#' foldList = generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subcascades = subcascades(predMap,thresh=0.7)
#' 
#' #create a Groupwise object
#' groupwise = groupwise(subcascades)
groupwise <- function(subcascades=NULL, maxCl=50)
{
    #################################################
    ##
    ## Check parameter 'subcascades'
    
    if(is.null(subcascades))
        return(NULL)
    
    if(!inherits(subcascades, 'Subcascades'))
        stop(errorStrings('subcascades'))
    
    #################################################
    ##
    ## Check parameter 'maxCl'

    if(!is.numeric(maxCl))
        stop(errorStrings('maxCl'))

    if(!maxCl>=1)
        stop(errorStrings('maxCl'))

    #################################################
    
    subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
        return(NULL)
        
    groupwise <- lapply(subcascades, function(casc){
        index <- sapply(rownames(casc), function(x){
            tmp<- numeric(maxCl)
            tmp[as.numeric(strsplit(x, '>')[[1]])+1]<-1
            return(tmp)
        })
        
        colnames(index) <- NULL
       
         index <- apply(index[rowSums(index)>0,,drop = FALSE],2,function(x){paste(x,collapse='')})
        
        
        res <- lapply( (split(x = 1:length(index),f = index)), function(x){
            casc[x,,drop = FALSE]
        })
        
        nms <- lapply( res, function(x){
            tmp<- logical(maxCl)
            tmp[as.numeric(strsplit(rownames(x)[1],'>')[[1]])+1]<- T
            paste(which(tmp)-1, collapse = '-')
        })
        
        names(res) <- nms
        
        return(res)
    })
    
    if(length(groupwise)==0)
    {
        return(NULL)
    }else{
        structure(list(groupings = groupwise,
                       maxCl = maxCl),
                  class = "Groupwise")
    }
}

#generic function for formatting outputs of a Groupwise object
format <- function(groupwise, ...) UseMethod("format")

#implementation of the generic function \code{\link{format}} to give an formatted output of a Groupwise output
format.Groupwise <- function(x, printSizes = length(x), ...) {
  if(printSizes > length(x)) {
    printSizes <- length(x)
  }
  
  cat("Grouwpise object ( max Cl = ", x$maxCl,") \n\n")
  print(x$groupings[1:printSizes], ...)
}