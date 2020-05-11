#' Calculation of a Confusion Table
#' 
#' Confusion table and class assignment of one cascade.
#' 
#' @inheritParams subcascades
#' @param cascade
#' A numeric vector of classes or a character string of type '1>2>3' of at least two class labels reflected in 'predictionMap'.
#' 
#' @param other.classes
#' This parameter can be either NULL, 'all' or a numeric vector of classes that are not part of the cascade parameter.
#' If other.classes is:
#' - NULL, only the cascade classes are evaluated. 
#' - 'all', all remaining classes are evaluated. 
#' -  a vector of classes, those classes are evaluated.
#' 
#' @param sort
#' If TRUE (default) the classes that are not part of cascade are sorted based on their confusion.
#' 
#' 
#' 
#' @return 
#' A confusion matrix of sensitivities, with the label of the predicted classes in the rows and the labels of the original class in the columns.
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data = esl$data
#' labels = esl$labels
#' foldList = generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' genMap = gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' 
#' # Calculation of the confusion matrix for '0>2>3>4'.
#' confusion.table(genMap, cascade = '0>2>3>4')
#' # Calculation of the confusion matrix for '0>2>3>4' 
#' # and the assignment of all samples of the other classes.
#' confusion.table(genMap, cascade = '0>2>3>4', 
#'                 other.classes='all', sort = TRUE)


confusion.table <- function(predictionMap=NULL, cascade = NULL, other.classes=NULL, sort = TRUE)
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
    classes <- unique(labs)
    
    if(is.null(cascade))
        stop(errorStrings('cascadeMissing'))
    
    if(!is.numeric(cascade) & !is.character(cascade))
        stop(errorStrings('cascade2'))
        
    if(is.character(cascade))
    {
        if(length(cascade)>1)
            stop(errorStrings('cascade2'))
            
        correct <- regexpr(pattern = '^([[:digit:]]+>)+[[:digit:]]+$', text = cascade)>0
        
        if(!correct)
            stop(errorStrings('cascade2'))
        
        cascade <- as.numeric(strsplit(cascade, '>')[[1]])
    }
    
    if(length(cascade)<2)
        stop(errorStrings('cascade2'))
    
    if(! all(cascade %in% classes))
        stop(errorStrings('cascade2'))
    
    #################################################
    ##
    ## Check parameter 'other.classes'

    if(!is.null(other.classes))
    {
        if(!is.numeric(other.classes) & !is.character(other.classes))
            stop(errorStrings('other.classes'))

        if(is.character(other.classes))
        {
            if(length(other.classes) != 1)
                stop(errorStrings('other.classes'))
                
            if(other.classes != 'all')
                stop(errorStrings('other.classes'))
                
                
            index <- !(classes %in% cascade)
            if(sum(index)>0)
            {
               other.classes = classes[index]
            }else{
               other.classes = NULL
            }
        }
    
        if(! all(other.classes %in% classes))
            stop(errorStrings('other.classes'))
        
        if( length(intersect(cascade, other.classes))>0)
            stop(errorStrings('other.classes'))
    }
    
    #################################################
    ##
    ## Check parameter 'sort'

        if( !is.logical(sort) | length(sort) !=1)
            stop(errorStrings('sort'))

    #################################################
    ###
    ### Generate confusion table of cascade internal classes .
    
    conf.tab.cascade <- confusion.table.intern(predictionMap, cascade = cascade, test.classes = cascade)
    
    if(is.null(other.classes))
    {
        class(conf.tab.cascade) <- 'ConfusionTable'
        return(conf.tab.cascade)
    }else{
        
        conf.tab.other.classes <- confusion.table.intern(predictionMap, cascade = cascade, test.classes = other.classes)
        
        #############################################################
        ###
        ### Sort other classes
        
        if(sort == TRUE)
        {
            score = apply(conf.tab.other.classes,2,function(x){
                sum(sapply(1:length(x),function(y){
                    (round(x[y]*100)*1000^(length(x)-y+1))
                }))
            })
            
            conf.tab.other.classes <- conf.tab.other.classes[,sort.int(score,decreasing = T,index.return = T)$ix,drop  =FALSE]
        }
        
        conf.tab <- cbind(conf.tab.cascade, conf.tab.other.classes)
        class(conf.tab) <- 'ConfusionTable'
        return(conf.tab)
    }
}
