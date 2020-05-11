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