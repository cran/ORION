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
