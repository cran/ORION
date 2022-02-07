#' Filters for size
#' 
#' Filters out the Subcascades object for the given sizes.
#' 
#' @inheritParams subcascades
#' @inheritParams summarySubcascades
#' 
#' @param size
#' A numeric value that defines the size of the cascades that should be returned. 
#' The smallest size is 2 and the largest the maximal number of classes of the current dataset.
#' 
#' @return 
#' A Subcascades object as in \code{\link{subcascades}}, that does not include cascades of the specific lengths that hve been filtered.
#' 
#' @seealso \code{\link{keepSize}}, \code{\link{dropSets}}, \code{\link{keepSets}}, \code{\link{dropThreshold}}, \code{\link{keepThreshold}}
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc = subcascades(predMap,thresh=0.7)
#' 
#' # filters out cascades that have a length of 3
#' dropSize(subc,size=3)
#' # filters out cascades that have a length of 3 or 4
#' dropSize(subc, size=c(3,4))
dropSize <- function(subcascades=NULL, size = NA)
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
    ## Check parameter 'size'
    
  if(all(is.na(size)))
    return(subcascades)
  
  if(!is.numeric(size)|any(is.na(size)))
    stop(errorStrings('size.na'))

    #################################################
    
    subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
        return(NULL)
    
    size <- paste('size.',unique(size), sep = '')
    
    keep <- !(names(subcascades) %in% size)
    
    if(sum(keep)==0)
        return(NULL)
    
    subcascades <- subcascades[keep]
    class(subcascades) <- 'Subcascades'
    
    return(subcascades)
}
