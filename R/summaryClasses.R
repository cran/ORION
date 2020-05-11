#' Occurrence of Classes by Size
#' 
#' \code{summaryClasses} returns the occurrence of classes by size
#' 
#' @inheritParams summarySubcascades
#' 
#' 
#' @details 
#' This function gives an overview of the classes of the Subcascades object. For each length in 
#' the Subcascades object the occurence of classes is given.
#' 
#' @return 
#' A matrix summarizing the overview characteristics of the Subcascades object.
#' 
#' @seealso \code{\link{subcascades}}, \code{\link{summarySubcascades}}, \code{\link{summaryGroupwise}}
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data = esl$data
#' labels = esl$labels
#' foldList = generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' genMap = gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subcascades = subcascades(genMap,thresh=0.7)
#' 
#' summaryClasses(subcascades)
#' 
#' 
summaryClasses <- function(subcascades=NULL)
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

    overview <- lapply(subcascades, function(casc){table(unlist(strsplit(rownames(casc), '>')))})
    
    max.classes <- max(as.numeric(unlist(lapply(overview, function(x){names(x)}))))+1
    
    overview <- sapply(overview, function(x){
        res <- numeric(max.classes)
        
        classes <- as.numeric(names(x))+1
        res[classes] <- as.vector(x)
        
        return(res)
    })
    
    if(!is.matrix(overview))
    {
        overview <- matrix(overview, nrow = 1)
    }else{
        overview <- t(overview)
    }
    
    colnames(overview) <- paste('cl.', 0:(max.classes-1), sep = '')
    rownames(overview) <- names(subcascades)
    
    return(overview)
}

