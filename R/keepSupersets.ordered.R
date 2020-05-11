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
