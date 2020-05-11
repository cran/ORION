#' Merge Subcascades
#' 
#' \code{mergeSubcascades} adds the cascades from subcascades2 to the subcascades1, that have not been part of subcascades1.
#' 
#' @param subcascades1
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length. 
#' The rownames show the class order and the entries the sensitivity for each position of the cascade.
#' @param subcascades2
#' A Subcascades object like subcascades1
#' 
#' 
#' @inherit subcascades return
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
#' # make two Subcascades objects
#' subcascades1 = subcascades(genMap, size = c(3,4), thresh = 0.6)
#' subcascades2 = subcascades(genMap, size = c(4), thresh = 0.5)
#' # add the cascades of subcascades2 to subcascades1
#' mergeSubcascades(subcascades1, subcascades2)


mergeSubcascades <- function(subcascades1=NULL, subcascades2=NULL){
    #################################################
    ##
    ## Check parameter 'subcascades1'
    
    if(!is.null(subcascades1) & !inherits(subcascades1, 'Subcascades'))
        stop(errorStrings('subcascades1'))

    #################################################
    ##
    ## Check parameter 'subcascades2'

    if(!is.null(subcascades2) & !inherits(subcascades2, 'Subcascades'))
        stop(errorStrings('subcascades2'))

    #################################################
   
    if(!is.null(subcascades1))
       subcascades1 <- subcascades1[sapply(subcascades1, function(x){!is.null(x)})]
   
   if(!is.null(subcascades2))
       subcascades2 <- subcascades2[sapply(subcascades2, function(x){!is.null(x)})]
   
    if(length(subcascades1)!=0)
    {
        result = subcascades1
        
        if(length(subcascades2)!=0)
        {
            for (size in names(subcascades2)){
                if (size %in% names(result)){
                    add.casc = !(rownames(subcascades2[[size]]) %in% rownames(result[[size]]))
                    result[[size]] = rbind(result[[size]],subcascades2[[size]][add.casc,])
                    Smin = apply(result[[size]],1,min)
                    result[[size]][order(Smin,decreasing=T),]
                } else {
                    result[[size]] = subcascades2[[size]]
                }
            }
            
            size.as.numeric <- as.numeric(sapply(names(result), function(x){strsplit(x,'.',fixed = TRUE)[[1]][2]}))
            result = result[order(size.as.numeric,decreasing = T)]
        }
    }else{
        result = subcascades2
    }
    

  class(result) = 'Subcascades'
  return(result)
}
