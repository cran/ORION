#' Calculation of Binary Classifier Sensitivities
#' 
#' Sensitivities of all pairwise binary classifiers for all classes.
#' 
#' @inheritParams subcascades
#' 
#' @details 
#' The Conf object contains all class sensitivities for each binary trained classifiers. 
#' The $fC-part is a column-vector and contains the sensitivities for the first class 
#' of each pairwise classifier. The rows stand for the pairwise classifiers, whereby 
#' 0vs1 means that this classifier was trained for class 0 against class 1, with 
#' class 0 being the first class. The number '-1' is used as placeholder. 
#' The $sC-part is a matrix and contains the preformance measures for all second classes, 
#' which are meant here as all classes except the first class. The rows correspond 
#' to the binary classifiers and the columns to the classes. Furthermore the 
#' foldlist ($foldList) used and the dataset name ($dataName) are saved as variable 
#' of the object.
#' 
#' 
#' @return 
#' Object of class Conf. For a detailed description refer to details sections.
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
#' gen.conf(genMap)


gen.conf <- function(predictionMap=NULL)
{
    #################################################
    ##
    ## Check parameter 'predictionMap'
    
    if(is.null(predictionMap))
        stop(errorStrings('predictionMapMissing'))
    
    if(!inherits(predictionMap, 'PredictionMap'))
        stop(errorStrings('predictionMap'))
    
    #################################################
    
    labs <- predictionMap$meta['label',]
    classes <- sort(unique(labs))

    combs <- expand.grid(classes,classes)[,2:1]

    tmp <- t(apply(combs,1,function(x){
        eval.cascades(predictionMap, cascades = c(x[1],x[2]))
    }))

    colnames(tmp) <- paste('TN', classes, sep = '')
    rownames(tmp) <- apply(combs,1,function(x){paste('[',x[1],'vs',x[2],']',sep = '')})

    fC <- c()
    for (i in 1:nrow(tmp))
    {
        if(combs[i,1] == combs[i,2])
        {
            tmp[i,] <- -1
        }
        firstClass <- which(classes == combs[i,1])
        fC <- c(fC, tmp[i, firstClass])
        tmp[i,firstClass] = -1
    }
    
    fC <- matrix(fC, ncol = 1)
    
    rownames(fC) <- rownames(tmp)
    colnames(fC) <- 'TP'
 
    result <- list( fC = fC,
                    sC = tmp,
                    foldList = predictionMap$foldList,
                    dataName = predictionMap$dataName)
                    
    class(result) <- 'Conf'
                    
    return(result)
}
