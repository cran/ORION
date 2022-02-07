#' Filter Subcascades
#' 
#' \code{keepSets} filters specific cascade sets.
#' 
#' @inheritParams summarySubcascades
#' 
#' @param sets 
#' Contains the set used for filtering. It is either a list of numeric vectors, a numeric vector, 
#' or a vector of characters representing a cascade of the following format '1>2>4'. Empty vectors are not allowed.
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
#' The parameter \code{neighbourhood} is not considered.
#' 
#' If \code{direction} is set to 'super' the Subcascades object is filtered for a superset of the given cascades. 
#' If the \code{type} parameter is set to 'any' each cascade is considered individually otherwise all cascades are used as reference for filtering.
#' A superset are cascades that are made up of (parts) of the given cascades (\code{ordered = TRUE}) or classes (\code{ordered=FALSE}) but can contain also more classes. 
#' Depending on the parameter \code{neighborhood} only classes defined in the \code{sets} parameter are allowed as neighbors ('direct').
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc = subcascades(predMap,thresh=0.7)
#' 
#' #define sets
#' set1 = list(c(1,2,3),c(2,3,4))
#' set2 = c('1>2>3','2>3>4')
#' 
#' # filter for the subset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' keepSets(subc, sets = set1, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' keepSets(subc, sets = set2, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' keepSets(subc, sets = set1, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' keepSets(subc, sets = set2, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain both the classes 
#' # {1,2,3} and {2,3,4} in exactly the given order, but allowing 
#' # for other classes inbetween
#' keepSets(subc, sets = set1, direction = 'super', 
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#' keepSets(subc, sets = set2, direction = 'super',
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#'          
#' # filter for the exact cascades
#' # sets can be a numeric list
#' result <- keepSets(subc, list(c(0,1,2),c(2,3,4,1)),
#'                    direction = 'exact', ordered=TRUE)
#' unlist(t(lapply(result,rownames)))
#' # or sets can be a character vector
#' result <- keepSets(subc, c('0>1>2','2>3>4>1'),
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
    
  if(is.null(sets) & length(sets)!=0)
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
  
  ##########################################
  
    if(direction %in% c('exact') && type == 'all')
      warning("If direction is set to \'exact\' the type parameter cannot be changed to \'all\'. \'any\' is used instead. ")
    
    ###############################################################################################################################

    return(switch( direction,
        'sub'    = keepSubsets(subcascades, sets , ordered ,  neighborhood , type ),
        'super'  = keepSupersets(subcascades, sets , ordered ,  neighborhood , type ),
        'exact'  = keepExact(subcascades, sets, ordered)))
}

keepSubsets <- function(subcascades=NULL, sets = NULL, ordered = FALSE,  neighborhood = 'direct', type = 'any')
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
  
  if(!is.logical(ordered))
    stop(errorStrings('ordered'))
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  
  
  ###############################################################################################################################
  if (ordered==FALSE){
    
    if((is.character(sets) & is.vector(sets))){
      sets = lapply(sets, function(x){as.numeric(strsplit(x,'>',fixed=T)[[1]])})
    }
    
    subcascades = keepSubsets.unorderd(subcascades, sets, neighborhood, type)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse='>')}))}))
    }
    
    subcascades = keepSubsets.orderd(subcascades, sets, neighborhood, type)
  }
  return(subcascades)
}

keepSubsets.unorderd <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  if(is.numeric(sets) & is.vector(sets))
    sets <- list(sets)
  
  if(!is.list(sets))
  {
    stop(errorStrings('sets.classes'))
  }else{
    if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
      stop(errorStrings('sets.classes'))
  }
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(x)})
  
  keep <- sizes.subcascades <= max(sizes.sets)
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  sets <- sapply(sets, function(x){paste(x,collapse = '>')})
  
  sets <- unique(sets)
  
  subcascades <- lapply(subcascades, function(casc){
    
    nms <- sapply(rownames(casc), function(x){
      x <- strsplit(x,'>')[[1]]
      numCl <- length(x)
      paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
    })
    
    nms <- paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',nms),'(>[[:digit:]]+)*$', sep = '')
    
    if(type == 'any')
    {
      keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
    }else{
      keep <- sapply(nms, function(exp){all(regexpr(pattern = exp, text = sets)>0)})
    }
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

keepSubsets.orderd <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
    stop(errorStrings('sets.cascades'))
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  sets <- unique(sets)
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(strsplit(x,'>', fixed =TRUE)[[1]])})
  
  keep <- sizes.subcascades <= max(sizes.sets)
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  subcascades <- lapply(subcascades, function(casc){
    nms <- paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',rownames(casc)),'(>[[:digit:]]+)*$', sep = '')
    
    if(type == 'any')
    {
      keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
    }else{
      keep <- sapply(nms, function(exp){all(regexpr(pattern = exp, text = sets)>0)})
    }
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

keepSupersets <- function(subcascades=NULL, sets = NULL, ordered = FALSE,  neighborhood = 'direct', type = 'any')
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
  
  if(!is.logical(ordered))
    stop(errorStrings('ordered'))
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  ###############################################################################################################################
  if (ordered==F){
    
    if((is.character(sets) & is.vector(sets))){
      classes = lapply(sets, function(x){as.numeric(strsplit(x,'>',fixed=T)[[1]])})
    }else{
      classes = sets
    }
    
    subcascades = keepSupersets.unordered(subcascades, classes, neighborhood, type)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse='>')}))}))
    }
    
    subcascades = keepSupersets.ordered(subcascades, sets, neighborhood, type)
  }
  return(subcascades)
}

keepSupersets.unordered <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  if(is.numeric(sets) & is.vector(sets))
    sets <- list(sets)
  
  if(!is.list(sets))
  {
    stop(errorStrings('sets.classes'))
  }else{
    if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
      stop(errorStrings('sets.classes'))
  }
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(x)})
  
  keep <- switch( neighborhood,
                  'indirect'     = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)},
                  'direct'  = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)})
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  sets <- sapply(sets, function(x){
    numCl <- length(x)
    paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
  })
  
  
  sets <- switch( neighborhood,
                  'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',sets),'(>[[:digit:]]+)*$', sep = ''),
                  'direct'  = paste('^([[:digit:]]+>)*',sets,'(>[[:digit:]]+)*$', sep = ''))
  
  sets <- unique(sets)
  
  subcascades <- lapply(subcascades, function(casc){
    tmp <- sapply(sets, function(exp){
      regexpr(pattern = exp, text = rownames(casc))>0
    })
    
    if(type == 'any')
    {
      keep <- apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,any)
    }else{
      keep <- apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,all)
    }
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

keepSupersets.ordered <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
    stop(errorStrings('sets.cascades'))
  
  #################################################
  ##
  ## Check parameter 'neighborhood'
  
  if(!(neighborhood %in% c('direct','indirect')))
    stop(errorStrings('neighborhood'))
  
  #################################################
  ##
  ## Check parameter 'type'
  
  if(!(type %in% c('all','any')))
    stop(errorStrings('type'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(strsplit(x,'>', fixed =TRUE)[[1]])})
  
  keep <- switch( neighborhood,
                  'indirect'     = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)},
                  'direct'  = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)})
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  sets <- switch( neighborhood,
                  'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',sets),'(>[[:digit:]]+)*$', sep = ''),
                  'direct'  = paste('^([[:digit:]]+>)*',sets,'(>[[:digit:]]+)*$', sep = ''))
  
  sets <- unique(sets)
  
  subcascades <- lapply(subcascades, function(casc){
    tmp <- sapply(sets, function(exp){
      regexpr(pattern = exp, text = rownames(casc))>0
    })
    
    if(type == 'any')
    {
      keep <- apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,any)
    }else{
      keep <- apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,all)
    }
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

keepExact <- function(subcascades=NULL, sets = NULL, ordered = FALSE)
{
  
  #################################################
  ##
  ## Check parameter 'subcascades'
  
  if(is.null(subcascades))
    return(NULL)
  
  if(!inherits(subcascades, "Subcascades"))
    stop(errorStrings("subcascades"))
  
  
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
      stop(errorStrings("sets"))
    }else{
      if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
        stop(errorStrings("sets"))
    }
  }
  
  ################################################
  
  
  
  if (ordered==FALSE){
    
    if((is.character(sets) & is.vector(sets))){
      sets = lapply(sets, function(x){as.numeric(strsplit(x,">",fixed=T)[[1]])})
    }
    
    subcascades = keepExact.unordered(subcascades, sets)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse=">")}))}))
    }
    
    subcascades = keepExact.ordered(subcascades, sets)
  }
  return(subcascades)
}

keepExact.unordered <- function(subcascades=NULL, sets = NULL)
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
  
  if(is.numeric(sets) & is.vector(sets))
    sets <- list(sets)
  
  if(!is.list(sets))
  {
    stop(errorStrings('sets.classes'))
  }else{
    if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
      stop(errorStrings('sets.classes'))
  }
  
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(x)})
  
  keep <- sizes.subcascades %in% sizes.sets
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  sets <- sapply(sets, function(x){paste(x,collapse = '>')})
  sets <- unique(sets)
  
  subcascades <- lapply(subcascades, function(casc){
    
    nms <- sapply(rownames(casc), function(x){
      x <- strsplit(x,'>')[[1]]
      numCl <- length(x)
      paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
    })
    
    nms <- paste('^',nms,'$', sep = '')
    
    keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

keepExact.ordered <- function(subcascades=NULL, sets = NULL)
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
    stop(errorStrings('sets.cascades'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  sets <- unique(sets)
  
  sizes.subcascades <- sapply(names(subcascades), function(x){as.numeric(strsplit(x,'.', fixed =TRUE)[[1]][2])})
  sizes.sets <- sapply(sets, function(x){length(strsplit(x,'>', fixed =TRUE)[[1]])})
  
  keep <- sizes.subcascades %in% sizes.sets
  
  if(sum(keep)==0)
  {
    return(NULL)
  }else{
    subcascades <- subcascades[keep]
  }
  
  subcascades <- lapply(subcascades, function(casc){
    nms <- paste('^',rownames(casc),'$', sep = '')
    
    
    keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
    
    if(sum(keep)==0)
    {
      return(NULL)
    }else{
      casc[keep,,drop=FALSE]
    }
  })
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}