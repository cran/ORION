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