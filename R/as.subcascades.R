#' Coerce to a Subcascades Object 
#' 
#' Converts from a Groupwise object to a Subcascades object.
#' 
#' @param groupwise
#' A Groupwise object, which comprises a two-leveled list. The first level collects cascades of the same size.
#' The second level contains a list of unique class combinations, labelled as a character string with '-' separating the different classes. 
#' For each unique set of class combinations the corresponding orders and their performance are given as a matrix, 
#' where each row contains a cascade, given as a character string of type '1>2>3', and the columns the sensitivity for the class at the corresponding position.
#' Each matrix is sorted row-wise according to the achieved minimal classwise sensitivites of the cascades (decreasing).
#' 
#' @details 
#' Converts a Groupwise object to a Subcascades object.
#' 
#' @inherit subcascades return
#' 
#' @seealso \code{\link{as.groupwise}}, \code{\link{as.edgedataframe}}
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
#' 
#' # generate a Groupwise object
#' subcascades = subcascades(genMap,thresh=0.7)
#' groupwise = as.groupwise(subcascades)
#' 
#' #convert it so a Subcascades object
#' as.subcascades(groupwise)
#' 
#' 
#'

as.subcascades <- function(groupwise=NULL)
{
    #################################################
    ##
    ## Check parameter 'groupwise'
    
    if(is.null(groupwise))
        return(NULL)
        
    if(!inherits(groupwise, 'Groupwise'))
        stop(errorStrings('groupwise'))
 
    #################################################
    
    groupwise <- groupwise[sapply(groupwise, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
    {
        return(NULL)
    }
    
    subcascades <- lapply(groupwise, function(size){
        casc <- do.call('rbind', size)
        min.sens <- sort(apply(casc,1,min), decreasing=TRUE)
        return(casc[names(min.sens),,drop = FALSE])
    })
    
    if(length(subcascades)==0)
    {
        return(NULL)
    }else{
        class(subcascades) <- 'Subcascades'
        return(subcascades)
    }
}
