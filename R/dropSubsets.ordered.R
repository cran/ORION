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
    
    change <- switch( neighborhood,
        'indirect'     = sizes.subcascades <= max(sizes.sets),
        'direct'  = sizes.subcascades <= max(sizes.sets))


    nms.sub <-names(subcascades)
    subcascades <- lapply(1:length(subcascades), function(i){
        casc <- subcascades[[i]]
        
        if (change[i]){
            nms <- switch( neighborhood,
                'indirect'     = paste('^([[:digit:]]+>)*',gsub('>', '(>[[:digit:]]+)*>',rownames(casc)),'(>[[:digit:]]+)*$', sep = ''),
                'direct'  = paste('^([[:digit:]]+>)*',rownames(casc),'(>[[:digit:]]+)*$', sep = ''))
        
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
