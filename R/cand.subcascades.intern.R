cand.subcascades.intern <- function(conf=NULL, thresh=0, size=1, numSol = 1000)
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

    if(!is.numeric(size) | length(size)!=1)
        stop(errorStrings('size2'))

    if(size<2)
        stop(errorStrings('size2'))
    
    #################################################
    ##
    ## Check parameter 'numSol'
    
    if(!is.numeric(numSol) | length(numSol)!=1)
    stop(errorStrings('numSol'))
    
    if(numSol<1)
    stop(errorStrings('numSol'))
    
    #################################################

    fC <- conf$fC
    sC <- conf$sC
    
    fC[fC < thresh] <- 0
    sC[sC < thresh] <- 0
    
    numCl <- ncol(sC)
    numMod<- nrow(sC)
    
    storage <- rep(-1, numSol*size)
    skip    <- 0
    
   	cobject <- .C(	'subcascades',
                    as.double(fC),
                    as.double(sC),
                    as.integer(numCl),
                    as.integer(numMod),
                    as.integer(numSol),
                    as.integer(size),
                    as.integer(numCl-size),
                    integer(1),
                    integer(1),
                    result = as.integer(storage),
                    skip = as.integer(skip))
                
    res  <- cobject$result
    skip <- cobject$skip
    
    if(skip > 0)
    {
        warning(paste('More than', numSol,'candidate cascades detected. Skipped',skip, 'candidate cascades.'))
    }
    
    if(length(res)==0)
    {
        return(NULL)
    }else{
        res <- matrix(res, nrow = size)
        res <- t(res[, apply(res,2,function(x){!all(x == -1)}),drop = FALSE])
        
        if(nrow(res)!= 0)
        {
            rownames(res) <- paste('size.',size,'_', 1:nrow(res), sep ='')
            colnames(res) <- paste('pos.', 1:ncol(res), sep ='')
            return(res)
        }else{
            return(NULL)
        }
    }
}

