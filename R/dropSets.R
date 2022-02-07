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
#' dropSets(subc, sets = set1, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' dropSets(subc, sets = set2, direction = 'sub', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain either the classes 
#' # {1,2,3} or {2,3,4} independent of the order, but neighbored
#' dropSets(subc, sets = set1, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' dropSets(subc, sets = set2, direction = 'super', 
#'          ordered = FALSE, neighborhood = 'direct')
#' 
#' # filter for the superset cascades that contain both the classes 
#' # {1,2,3} and {2,3,4} in exactly the given order, but allowing 
#' # for other classes inbetween
#' dropSets(subc, sets = set1, direction = 'super', 
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#' dropSets(subc, sets = set2, direction = 'super',
#'         ordered = TRUE, neighborhood = 'indirect', type = 'all')
#'          
#' # filter for the exact cascades
#' # sets can be a numeric list
#' result <- dropSets(subc, list(c(0,1,2),c(2,3,4,1)),
#'                    direction = 'exact', ordered=TRUE)
#' unlist(t(lapply(result,rownames)))
#' # or sets can be a character vector
#' result <- dropSets(subc, c('0>1>2','2>3>4>1'),
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
        
  if(is.null(sets) & length(sets)!=0)
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

dropSubsets <- function(subcascades=NULL, sets = NULL, ordered = FALSE,  neighborhood = 'direct', type = 'any')
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
    
    subcascades = dropSubsets.unordered(subcascades, sets, neighborhood, type)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse='>')}))}))
    }
    
    subcascades = dropSubsets.ordered(subcascades, sets, neighborhood, type)
  }
  return(subcascades)
}

dropSubsets.unordered <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  change <- sizes.subcascades <= max(sizes.sets)
  
  sets <- sapply(sets, function(x){paste(x,collapse = '>')})
  
  sets <- unique(sets)
  
  nms.sub <-names(subcascades)
  subcascades <- lapply(1:length(subcascades), function(i){
    casc <- subcascades[[i]]
    
    if(change[i])
    {
      nms <- sapply(rownames(casc), function(x){
        x <- strsplit(x,'>')[[1]]
        numCl <- length(x)
        paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
      })
      
      nms <- paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',nms),'(>[[:digit:]]+)*$', sep = '')
      
      if(type == 'any')
      {
        keep <- !sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
      }else{
        keep <- !sapply(nms, function(exp){all(regexpr(pattern = exp, text = sets)>0)})
      }
      
      if(sum(keep)==0)
      {
        return(NULL)
      }else{
        casc[keep,,drop=FALSE]
      }
    }else{
      casc
    }
  })
  names(subcascades) <- nms.sub
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

dropSubsets.ordered <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  
  change <- sizes.subcascades <= max(sizes.sets)
  
  
  nms.sub <-names(subcascades)
  subcascades <- lapply(1:length(subcascades), function(i){
    casc <- subcascades[[i]]
    
    if (change[i]){
      nms <- paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',rownames(casc)),'(>[[:digit:]]+)*$', sep = '')
      
      if(type == 'any')
      {
        keep <- !sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
      }else{
        keep <- !sapply(nms, function(exp){all(regexpr(pattern = exp, text = sets)>0)})
      }
      
      if(sum(keep)==0)
      {
        return(NULL)
      }else{
        casc[keep,,drop=FALSE]
      }
    }else{
      casc
    }
  })
  names(subcascades) <- nms.sub
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

dropSupersets <- function(subcascades=NULL, sets = NULL, ordered = F,  neighborhood = 'direct', type = 'any')
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
    
    subcascades = dropSupersets.unordered(subcascades, classes, neighborhood, type)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse='>')}))}))
    }
    
    subcascades = dropSupersets.ordered(subcascades, sets, neighborhood, type)
  }
  return(subcascades)
}

dropSupersets.unordered <- function(subcascades, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  change <- switch( neighborhood,
                    'indirect'     = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)},
                    'direct'  = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)})
  
  sets <- sapply(sets, function(x){
    numCl <- length(x)
    paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
  })
  
  sets <- switch( neighborhood,
                  'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',sets),'(>[[:digit:]]+)*$', sep = ''),
                  'direct'  = paste('^([[:digit:]]+>)*',sets,'(>[[:digit:]]+)*$', sep = ''))
  
  sets <- unique(sets)
  
  nms.sub <-names(subcascades)
  subcascades <- lapply(1:length(subcascades), function(i){
    casc <- subcascades[[i]]
    
    if(change[i])
    {
      tmp <- sapply(sets, function(exp){
        regexpr(pattern = exp, text = rownames(casc))>0
      })
      
      if(type == 'any')
      {
        keep <- !apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,any)
      }else{
        keep <- !apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,all)
      }
      
      if(sum(keep)==0)
      {
        return(NULL)
      }else{
        casc[keep,,drop=FALSE]
      }
    }else{
      casc
    }
  })
  names(subcascades) <- nms.sub
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

dropSupersets.ordered <- function(subcascades=NULL, sets = NULL, neighborhood = 'direct', type = 'any')
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
  
  change <- switch( neighborhood,
                    'indirect'     = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)},
                    'direct'  = if(type == 'any'){sizes.subcascades >= min(sizes.sets)}else{sizes.subcascades >= max(sizes.sets)})
  
  sets <- switch( neighborhood,
                  'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',sets),'(>[[:digit:]]+)*$', sep = ''),
                  'direct'  = paste('^([[:digit:]]+>)*',sets,'(>[[:digit:]]+)*$', sep = ''))
  
  sets <- unique(sets)
  
  nms.sub <-names(subcascades)
  subcascades <- lapply(1:length(subcascades), function(i){
    casc <- subcascades[[i]]
    
    if (change[i]){
      tmp <- sapply(sets, function(exp){
        regexpr(pattern = exp, text = rownames(casc))>0
      })
      if(type == 'any')
      {
        keep <- !apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,any)
      }else{
        keep <- !apply(matrix(tmp, nrow = nrow(casc), ncol = length(sets)),1,all)
      }
      
      if(sum(keep)==0)
      {
        return(NULL)
      }else{
        casc[keep,,drop=FALSE]
      }
    }else{
      casc
    }
  })
  names(subcascades) <- nms.sub
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

dropExact <- function(subcascades=NULL, sets = NULL, ordered = FALSE)
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
    return(subcascades)
  
  
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
  
  ##################################### 
  
  
  
  if (ordered==FALSE){
    
    if((is.character(sets) & is.vector(sets))){
      sets = lapply(sets, function(x){as.numeric(strsplit(x,">",fixed=T)[[1]])})
    }
    
    subcascades = dropExact.unordered(subcascades, sets)
    
  }else{
    
    if(is.numeric(sets) & is.vector(sets)){
      sets <- list(sets)
    }
    
    if(all(sapply(sets, function(x){is.numeric(x)&is.vector(x)}))){
      sets = unlist(lapply(sets, function(x){unlist(lapply(sets, function(x){paste(x,collapse=">")}))}))
    }   
    
    subcascades = dropExact.ordered(subcascades, sets)
  }
  return(subcascades)
}

dropExact.unordered <- function(subcascades=NULL, sets = NULL)
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
  
  change <- sizes.subcascades %in% sizes.sets
  
  sets <- sapply(sets, function(x){paste(x,collapse = '>')})
  
  sets <- unique(sets)
  
  nms.sub <-names(subcascades)
  
  
  subcascades <- lapply(1:length(subcascades), function(i){
    casc <- subcascades[[i]]
    
    if(change[i])
    {
      nms <- sapply(rownames(casc), function(x){
        x <- strsplit(x,'>')[[1]]
        numCl <- length(x)
        paste(rep(paste('(',paste(x,collapse = '|'),'){1}',sep = ''), numCl), collapse = '>',sep = '')
      })
      nms <- paste('^',nms,'$', sep = '')
      keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
      
      
      if(sum(keep)==0)
      {
        return(casc)
      }else{
        casc = casc[!keep,,drop=FALSE]
        if(nrow(casc)==0){
          return(NULL)
        }
        return(casc)
      }
    }else{
      casc
    }
  })
  names(subcascades) <- nms.sub
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    class(subcascades) <- 'Subcascades'
    return(subcascades)
  }
}

dropExact.ordered <- function(subcascades=NULL, sets = NULL)
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
    stop(errorStrings('sets.cascades'))
  
  ##################################################
  
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
    return(NULL)
  
  subcascades <- lapply(subcascades, function(casc){
    nms <- paste('^',rownames(casc),'$', sep = '')
    
    keep <- sapply(nms, function(exp){any(regexpr(pattern = exp, text = sets)>0)})
    
    if(sum(keep)==0)
    {
      return(casc)
    }else{
      casc = casc[!keep,,drop=FALSE]
      if(nrow(casc)==0){
        return(NULL)
      }
      return(casc)
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