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