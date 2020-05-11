#' Filter Subcascades
#' 
#' \code{keepSets} filters specific cascade sets.
#' 
#' @inheritParams summarySubcascades
#' 
#' @param sets 
#' Contains the set used for filtering. It is either a list of numberic vectors, a numeric vector, 
#' or a vector of characters representing a cascade of the following format '1>2>4'.
#' @param direction
#' Either 'sub','super' or 'exact', indicating whether the subset, the superset or the exact set is filtered. 
#' The 'exact' case filters exactly the cascades defined in \code{sets}. Please refer to the details section for a detailed description. 
#' @param ordered
#' Either TRUE or FALSE indicating whether the order of the classes in sets is considered or not.
#' @param neighborhood
#' Either 'direct' or 'indirect' defines whether the given classes have to be direct neighbors ('direct') or there are other classes allowed inbetween ('indirect').
#' @param type
#' If more than one cascade is defined in sets, this parameter defines whether the filtered cascade has to
#' fit to at least one of the sets ('any') or to all of the given sets ('all').
#' 
#' @details
#' This function allows for filtering the Subcascades object. 
#' 
#' If \code{direction} is set to 'exact' the parameter \code{neighborhood} is ignored and the parameter \code{type} is used as its default and cannot be changed.
#' There has to be an exact match between the classes of the sets parameter cascades and the cascade of the Subcascades object, 
#' so not more but also not less classes are allowed. If the \code{ordered} parameter is set to TRUE also the order of the classes within the 
#' \code{sets} parameter and within the Subcascades object has be be exactly the same.
#' 
#' If \code{direction} is set to 'sub' the Subcascades object is filtered for subsets of the given cascades. 
#' If the \code{type} parameter is set to 'any' each cascade is considered individually otherwise all cascades are used as reference for filtering.
#' This means that either for each cascade of the \code{sets} parameter individually or for taking all together the Subcascades object is filtered for cascades that are 
#' made up of the same classes, a subset of classes (\code{ordered = FALSE}) or a cascade, part of a cascade (\code{ordered = TRUE}), 
#' resulting in a set of cascades that might contain less classes.
#' Depending on the parameter \code{neighborhood} each class has to have the same direct neighbors as in the \code{sets} parameter defined ('direct').
#' If \code{neighborhood} is set to 'indirect' the filtering is less strict and the direct neighborhood defined in \code{sets} is not considered.
#' 
#' If \code{direction} is set to 'super' the Subcascades object is filtered for a superset of the given cascades. 
#' If the \code{type} parameter is set to 'any' each cascade is considered individually otherwise all cascades are used as reference for filtering.
#' A superset are cascades that are made up of (parts) of the given cascades (\code{ordered = TRUE}) or classes (\code{ordered=FALSE}) but can contain also more classes. 
#' Depending on the parameter \code{neighborhood} each class has to have the same direct neighbors as in the \code{sets} parameter defined ('direct').
#' If \code{neighborhood} is set to 'indirect' the filtering is less strict and the direct neighborhood defined in \code{sets} is not considered.
#' 
#' 
#' @inherit subcascades return
#' 
#' @seealso \code{\link{dropSize}}, \code{\link{keepSize}}, \code{\link{dropSets}}, \code{\link{dropThreshold}}, \code{\link{keepThreshold}}
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
#' #define sets
#' set1 = list(c(1,2,3),c(2,3,4))
#' set2 = c('1>2>3','2>3>4')
#' 
#' # filter for the subset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' keepSets(subcascades, sets = set1, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' keepSets(subcascades, sets = set2, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' keepSets(subcascades, sets = set1, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' keepSets(subcascades, sets = set2, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain both the classes 
#' # {1,2,3} and {2,3,4} in exactly the given order, but allowing 
#' # for other classes inbetween
#' keepSets(subcascades, sets = set1, direction = 'super', 
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#' keepSets(subcascades, sets = set2, direction = 'super',
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#'          
#' # filter for the exact cascades
#' # sets can be a numeric list
#' result <- keepSets(subcascades, list(c(0,1,2),c(2,3,4,1)),
#'                    direction = 'exact', ordered=TRUE)
#' unlist(t(lapply(result,rownames)))
#' # or sets can be a character vector
#' result <- keepSets(subcascades, c('0>1>2','2>3>4>1'),
#'                    direction = 'exact',ordered=TRUE)
#' unlist(t(lapply(result,rownames)))         
#'  
keepSets <- function(subcascades=NULL, sets = NULL, direction = 'sub', ordered = FALSE,  neighborhood = 'direct', type = 'any')
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
    ## Check parameter 'sets'
    
  if(is.null(sets))
    return(subcascades)
  
  if(!(is.character(sets) & is.vector(sets)))
  {
    if(is.numeric(sets) & is.vector(sets))
      sets <- list(sets)
    
    if(!is.list(sets))
    {
      stop(errorStrings('sets'))
    }else{
      if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
        stop(errorStrings('sets'))
    }
  }
    
    #################################################
    ##
    ## Check parameter 'ordered'
    
    if(!(direction %in% c('sub','super','exact')))
        stop(errorStrings('direction'))
    
    #################################################
    ##
    ## Check parameter 'ordered'
    
    if(!is.logical(ordered))
        stop(errorStrings('ordered'))
    
    #################################################
    ##
    ## Check parameter 'type'
    
    if(!(type %in% c('all','any')))
        stop(errorStrings('type'))
    
    #################################################
    ##
    ## Check parameter 'neighborhood'
    
    if(!(neighborhood %in% c('direct','indirect')))
        stop(errorStrings('neighborhood'))
    
    ###############################################################################################################################

    return(switch( direction,
        'sub'    = keepSubsets(subcascades, sets , ordered ,  neighborhood , type ),
        'super'  = keepSupersets(subcascades, sets , ordered ,  neighborhood , type ),
        'exact'  = keepExact(subcascades, sets, ordered)))
}
