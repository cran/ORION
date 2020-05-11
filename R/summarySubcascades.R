#' Overview Subcascades
#' 
#' \code{summarySubcascades} returns a summarizing overview. For each length the number of cascades and the minimal class-wise sensitivity is given.
#' 
#' @param subcascades 
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length and 
#' is sorted row-wise according to the achieved minimal classwise sensitivities of the cascades (decreasing).
#' The rownames show the class order by a character string of type '1>2>3' and the entries the sensitivity for each position of the cascade.
#' 
#' 
#' @details 
#' This function gives an overview of the Subcascades object. For each length in 
#' the Subcascades object the number of cascades of that length, as well as their 
#' minimal classwise sensitivity is given. 
#' 
#' @return 
#' A matrix summarizing the overview characteristics of the Subcascades object. 
#' For each size (rows) the number of cascades within the Subcascades object (number) and the minimal classwise sensitivity (min.class.sens) are given.
#' 
#' @seealso \code{\link{plot.Subcascades}}, \code{\link{subcascades}}, \code{\link{summaryGroupwise}}, \code{\link{summaryClasses}}
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data <- esl$data
#' labels <- esl$labels
#' foldList <- generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' genMap <- gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' 
#' # use default parameter settings
#' subcascades <- subcascades(genMap,thresh=0.7)
#' summarySubcascades(subcascades)


summarySubcascades <- function(subcascades=NULL)
{
    #################################################
    ##
    ## Check parameter 'subcascades'
    
    if(is.null(subcascades))
        return(NULL)
    
    if(!inherits(subcascades, 'Subcascades'))
        stop(errorStrings('subcascades'))
    
    #################################################
    
    subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
    {
        return(NULL)
    }

    overview <- sapply(subcascades, function(casc){c(nrow(casc), min(casc))})
    
    if(!is.matrix(overview))
    {
        overview <- matrix(overview, nrow = 1)
    }else{
        overview <- t(overview)
    }
    
    colnames(overview) <- c('number', 'min.class.sens')
    rownames(overview) <- names(subcascades)
    
    return(overview)
}

