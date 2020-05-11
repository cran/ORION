#' Summary Subcascades Characteristics
#' 
#' Generates a general overview of the characteristics of the Subcascades object.
#' 
#' @param object 
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length and 
#' is sorted row-wise according to the achieved minimal classwise sensitivities of the cascades (decreasing).
#' The rownames show the class order by a character string of type '1>2>3' and the entries the sensitivity for each position of the cascade.
#' @param digits
#' Integer defining the number of decimal places as it is used in the \code{round}-function.
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives a general overview of characteristics of the Subcascades object, like number of cascades or maximal cascade length.
#' 
#' @return 
#' An invisible Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The function is called to print a summary of the Subcascades object to console.
#'
#' 
#' @seealso \code{\link{subcascades}}, \code{\link{summarySubcascades}}, \code{\link{summaryGroupwise}}, \code{\link{summaryClasses}}
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data <- esl$data
#' labels <- esl$labels
#' foldList <- generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' genMap <- gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subcascades <- subcascades(genMap,thresh=0.7,numSol=10000)
#' 
#' summary(subcascades)
#' 

summary.Subcascades <- function(object=NULL,  digits = 3, ...)
{
    #################################################
    ##
    ## Check parameter 'subcascades'
    
    if(!inherits(object, 'Subcascades'))
        stop(errorStrings('subcascades'))

    #################################################
    ##
    ## Check parameter 'digits'
    
    if(!is.numeric(digits) | length(digits)!=1)
        stop(errorStrings('digits'))

    if(digits<1)
        stop(errorStrings('digits'))

    #################################################

    ovS <- summarySubcascades(object)
    ovC <- summaryClasses(object)
    size<- strsplit(names(object)[1],'\\.')[[1]][2]

    allOvC <- colSums(ovC)
    nms.allOvC <- names(allOvC)
    allOvC <- matrix(allOvC, nrow=1)
    rownames(allOvC) <- 'all'
    colnames(allOvC) <- nms.allOvC

    cat('-------------------------------------------------------------------------------\n')
    cat('Overall:\n')
    cat(paste('Number of cascades (number): ',sum(ovS[,'number']),'\n', sep = ''))
    cat(paste('Minimal classwise sensitivity (min.class.sens): ',round(mean(ovS[,'min.class.sens']), digits = digits),'\n', sep = ''))
    cat(paste('Maximal cascades length: ',size,'\n', sep = ''))
    cat(paste('Number of involved classes: ',ncol(allOvC),'\n', sep = ''))
    cat(paste('\nClasses: \n', sep = ''))
    print(colnames(allOvC))
     
    cat('-------------------------------------------------------------------------------\n')
    cat(paste('Sorted by size:\n\n', sep = ''))
         
    print(round(ovS, digits = digits))
    cat('---------------------------------------------\n')
    cat(paste('Occurrence of classes:\n\n', sep = ''))
    print(allOvC)
    cat(paste('\nOccurrence of classes by size:\n\n', sep = ''))
    print(ovC)
    cat('---------------------------------------------\n')
    cat(paste('Longest cascades(size=',size,'):\n\n', sep = ''))
    print(round(object[[1]], digits = digits))
    cat('-------------------------------------------------------------------------------\n')
    return(invisible(object))
}
