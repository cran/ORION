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
    
    change <- switch( neighborhood,
        'indirect'     = sizes.subcascades <= max(sizes.sets),
        'direct'  = sizes.subcascades <= max(sizes.sets))
    
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
        
            nms <- switch( neighborhood,
                'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',nms),'(>[[:digit:]]+)*$', sep = ''),
                'direct'  = paste('^([[:digit:]]+>)*',nms,'(>[[:digit:]]+)*$', sep = ''))
      
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
