confusion.table.intern <- function(predictionMap = NULL, cascade = NULL, test.classes = NULL)
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
    ## Check parameter 'cascade'
    
    labs <- predictionMap$meta['label',]
    pred <- predictionMap$pred
    
    classes <- unique(labs)
    
    if(is.null(cascade))
        stop(errorStrings('cascadeMissing'))
    
    if(!is.numeric(cascade))
        stop(errorStrings('cascade'))
    
    if(length(cascade)<2)
        stop(errorStrings('cascade'))
    
    if(!all(cascade %in% classes))
        stop(errorStrings('cascade'))
    
    #################################################
    ##
    ## Check parameter 'test.classes'
    
    if(!is.numeric(test.classes))
        stop(errorStrings('test.classes'))
    
    if(! all(test.classes %in% classes))
        stop(errorStrings('test.classes'))
        
    #############################################################
    ###
    ### Generate confusion table.
    
    conf.tab <- sapply(test.classes, function(cl){
        conf.tab.one.class <- rep(0, length(cascade))
        index <- which(labs == cl)
        numCl <- length(index)
        for(i in 1:(length(cascade)-1))
        {
            cl1 <- cascade[i]
            cl2 <- cascade[i+1]
            act.pred <- paste('[',cl1,'vs',cl2,']',sep = '')
            if(length(index)==0)
            {
                break
            }else{
                ind2 <- which(pred[act.pred,index]==cl1)
                conf.tab.one.class[which(cascade==cl1)] <- length(ind2)
                if(length(ind2)>0)
                {
                    index <- index[-ind2]
                }
                if(i == length(cascade)-1)
                {
                    ind2 <- which(pred[act.pred,index]==cl2)
                    conf.tab.one.class[which(cascade==cl2)] <- length(ind2)
                }
            }
        }
        conf.tab.one.class[is.na(conf.tab.one.class)] <- 0
        return(conf.tab.one.class/numCl)
    })
    
    if(!is.matrix(conf.tab))
    {
        conf.tab <- matrix(conf.tab, nrow = length(test.classes), ncol = length(cascade))
    }
    
    rownames(conf.tab) <- paste('pred.', cascade, sep = '')
    colnames(conf.tab) <- paste('cl.', test.classes, sep = '')
    
    return(conf.tab)
}
