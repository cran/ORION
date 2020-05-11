cand.subcascades<- function(conf=NULL, thresh=0, size=NULL, numSol = 1000)
{
    #################################################
    ##
    ## Check parameter 'conf'

    if(is.null(conf))
        stop(errorStrings('confMissing'))
    
    if(!inherits(conf, 'Conf'))
        stop(errorStrings('conf'))
    
    #################################################
    ##
    ## Check parameter 'thresh'
    
    if(!is.numeric(thresh) | length(thresh)!=1)
        stop(errorStrings('thresh'))
    
    if(thresh<0 | thresh>1 )
        stop(errorStrings('thresh'))
    
    #################################################
    ##
    ## Check parameter 'size'

    if(is.null(size))
        size <- sqrt(nrow(conf$fC)):2

    if(!is.numeric(size))
        stop(errorStrings('size'))

    if(any(size<2))
        stop(errorStrings('size'))

    #################################################
    ##
    ## Check parameter 'numSol'

    if(!is.numeric(numSol) | length(numSol)!=1)
        stop(errorStrings('numSol'))

    if(numSol<1)
        stop(errorStrings('numSol'))

    #################################################

    size <- sort(unique(size), decreasing=T)

    result <- lapply(size, function(x){
        cand.subcascades.intern(conf, thresh=thresh, size=x, numSol = numSol)
    })

    names(result) <- paste('size.', size, sep = '')

    return(result)
}

