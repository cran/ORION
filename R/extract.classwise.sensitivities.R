extract.classwise.sensitivities <- function(predictionMap=NULL, cascades=NULL)
{
    #################################################
    ##
    ## Check parameter 'predictionMap'
    
    if(is.null(predictionMap))
        stop(errorStrings('predictionMapMissing'))
    
    if(!inherits(predictionMap, 'PredictionMap'))
        stop(errorStrings('predictionMap'))
    
    #################################################
    ##
    ## Check parameter 'cascades'
    
    if(is.null(cascades))
        return(NULL)
    
    if(!is.numeric(cascades))
    {
         stop(errorStrings('cascades'))
    }else{
        if(!is.matrix(cascades))
        {
            if(is.vector(cascades))
            {
                cascades <- matrix(cascades, nrow = 1)
            }else{
                stop(errorStrings('cascades'))
            }
        }
    }
    
    #################################################

    labs <- predictionMap$meta['label',]
    pred <- predictionMap$pred

    classes <- unique(labs)
    
    if(!all(unique(as.vector(cascades)) %in% classes))
    {
        stop(errorStrings('cascades'))
    }
    
    result <- t(apply(cascades,1,function(casc){
        sapply(casc, function(cl){
            index <- which(labs == cl)
            numCl <- length(index)
            for(i in 1:(length(casc)-1))
            {
                cl1 <- casc[i]
                cl2 <- casc[i+1]
            
                act.pred <- paste('[',cl1,'vs',cl2,']',sep = '')

                if(length(index)==0)
                {
                    numCorrect <- 0
                    break
                }else{
                    if(cl1 == cl)
                    {
                        ind2 <- which(pred[act.pred,index]==cl1)
                        index <- index[ind2]
                        numCorrect <- length(index)
                        break
                    }else{
                        ind2 <- which(pred[act.pred,index]==cl2)
                        index <- index[ind2]
                        numCorrect <- length(index)
                    }
                }
            
            }
        
            return(numCorrect/numCl)
        })
    }))
    
    if(!is.matrix(result))
    {
        result <- matrix(result,nrow = 1)
    }
    
    rownames(result) <- apply(cascades,1,function(x){
        paste(x, collapse = '>')
    })
    
    colnames(result) <- paste('pos.', 0:(ncol(cascades)-1), sep = '')
    
    return(result)
}

