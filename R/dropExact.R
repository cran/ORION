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