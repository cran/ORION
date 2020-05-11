#' Overview Class Groups
#' 
#' \code{summaryGroupwise} returns a summarizing overview. For each length the number of permutations consisting of the same set of classes is given.
#' 
#' @inheritParams summarySubcascades
#' @param maxCl 
#' An integer defining the lower bound for the maximal number of classes. Has only to be set if the analyzed dataset has more than 50 classes.
#' 
#' 
#' @details 
#' This function gives an overview of the subgroup characteristics of the Subcascades object. 
#' The number of permutations per size is given. A permutation means that the corresponding cascades contain the same classes but with different order.
#' 
#' @return 
#' A matrix summarizing the overview characteristics of the Groupwise object.
#' 
#' @seealso \code{\link{subcascades}}, \code{\link{summarySubcascades}}, \code{\link{summaryClasses}}
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
#' summaryGroupwise(subcascades)
#'


summaryGroupwise <- function(subcascades=NULL, maxCl =50)
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
    class(subcascades) <- 'Subcascades'
    
    if(length(subcascades)<1)
        return(NULL)
        
    overview <- sapply(as.groupwise(subcascades, maxCl=maxCl), function(size){
        
        tab <- table(sapply(size, function(cw){
            if(is.null(cw))
            {
                return(0)
            }else{
                return(nrow(cw))
            }
        }))
        
        res <- rep(0, maxCl*100)
        res[as.numeric(names(tab))] <- as.vector(tab)
        
        return(res)
    })
    
    
    if(!is.matrix(overview))
    {
        overview <- matrix(overview, nrow = maxCl, ncol = length(subcascades))
    }
    
    overview <- t(overview)
    
    colnames(overview) <- paste('perm.',1:ncol(overview),sep = '')
    rownames(overview) <- names(subcascades)
    
    overview <- overview[, colSums(overview)>0,drop=FALSE]
    
    return(overview)
}
