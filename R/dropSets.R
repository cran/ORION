#' Filter out Subcascades
#' 
#' \code{dropSets} filters out specific cascade sets.
#' 
#' @inherit keepSets 
#' 
#' @seealso \code{\link{dropSize}}, \code{\link{keepSize}}, \code{\link{keepSets}}, \code{\link{dropThreshold}}, \code{\link{keepThreshold}}
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
#' dropSets(subcascades, sets = set1, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' dropSets(subcascades, sets = set2, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' dropSets(subcascades, sets = set1, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' dropSets(subcascades, sets = set2, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain both the classes 
#' # {1,2,3} and {2,3,4} in exactly the given order, but allowing 
#' # for other classes inbetween
#' dropSets(subcascades, sets = set1, direction = 'super', 
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#' dropSets(subcascades, sets = set2, direction = 'super',
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#'          
#' # filter for the exact cascades
#' # sets can be a numeric list
#' result <- dropSets(subcascades, list(c(0,1,2),c(2,3,4,1)),
#'                    direction = 'exact', ordered=TRUE)
#' unlist(t(lapply(result,rownames)))
#' # or sets can be a character vector
#' result <- dropSets(subcascades, c('0>1>2','2>3>4>1'),
#'                    direction = 'exact',ordered=TRUE)
#' unlist(t(lapply(result,rownames)))         


dropSets <- function(subcascades=NULL, sets = NULL, direction = 'sub', ordered = FALSE,  neighborhood = 'direct', type = 'any')
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
    return(NULL)
  
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
        'sub'    = dropSubsets(subcascades, sets , ordered ,  neighborhood , type ),
        'super'  = dropSupersets(subcascades, sets , ordered ,  neighborhood , type ),
        'exact'  = dropExact(subcascades, sets, ordered )))
}
