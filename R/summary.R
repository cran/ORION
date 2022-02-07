#' Overview Subcascades
#' 
#' \code{summarySubcascades} returns a summarizing overview. For each length the number of cascades and the minimal class-wise sensitivity is given.
#' 
#' @param subcascades 
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length and 
#' is sorted row-wise according to the achieved minimal classwise sensitivities of the cascades (decreasing).
#' The rownames show the class order by a character string of type '1>2>3' and the entries the sensitivity for each position of the cascade.
#' 
#' 
#' @details 
#' This function gives an overview of the Subcascades object. For each length in 
#' the Subcascades object the number of cascades of that length, as well as their 
#' minimal classwise sensitivity is given. 
#' 
#' @return 
#' A matrix summarizing the overview characteristics of the Subcascades object. 
#' For each size (rows) the number of cascades within the Subcascades object (number) and the minimal classwise sensitivity (min.class.sens) are given.
summarySubcascades <- function(subcascades=NULL)
{
  #################################################
  ##
  ## Check parameter 'subcascades'
  
  if(is.null(subcascades))
    return(NULL)
  
  if(!inherits(subcascades, 'Subcascades'))
    stop(errorStrings('subcascades'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }
  
  overview <- sapply(subcascades, function(casc){c(nrow(casc), min(casc))})
  
  if(!is.matrix(overview))
  {
    overview <- matrix(overview, nrow = 1)
  }else{
    overview <- t(overview)
  }
  
  colnames(overview) <- c('number', 'min.class.sens')
  rownames(overview) <- names(subcascades)
  
  return(overview)
}

#' Occurrence of Classes by Size
#' 
#' \code{summaryClasses} returns the occurrence of classes by size
#' 
#' @inheritParams summarySubcascades
#' 
#' @details 
#' This function gives an overview of the classes of the Subcascades object. For each length in 
#' the Subcascades object the occurence of classes is given.
#' 
#' @return 
#' A matrix summarizing the overview characteristics of the Subcascades object.
summaryClasses <- function(subcascades=NULL)
{
  #################################################
  ##
  ## Check parameter 'subcascades'
  
  if(is.null(subcascades))
    return(NULL)
  
  if(!inherits(subcascades, 'Subcascades'))
    stop(errorStrings('subcascades'))
  
  #################################################
  
  subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }
  
  overview <- lapply(subcascades, function(casc){table(unlist(strsplit(rownames(casc), '>')))})
  
  max.classes <- max(as.numeric(unlist(lapply(overview, function(x){names(x)}))))+1
  
  overview <- sapply(overview, function(x){
    res <- numeric(max.classes)
    
    classes <- as.numeric(names(x))+1
    res[classes] <- as.vector(x)
    
    return(res)
  })
  
  if(!is.matrix(overview))
  {
    overview <- matrix(overview, nrow = 1)
  }else{
    overview <- t(overview)
  }
  
  colnames(overview) <- paste('cl.', 0:(max.classes-1), sep = '')
  rownames(overview) <- names(subcascades)
  
  return(overview)
}

#' Summary Subcascades Characteristics
#' 
#' Generates a general overview of the characteristics of the Subcascades object.
#' 
#' @param object 
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#'
#' @param includeClassSummary
#' Boolean indicating if the occurrence of classes by size should be included in the summary.
#'
#' @param digits
#' Integer defining the number of decimal places as it is used in the \code{round}-function.
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives a general overview of characteristics of the Subcascades object, like number of cascades or maximal cascade length.
#'
#' @seealso \code{\link{subcascades}}, \code{\link{summary.PredictionMap}}, \code{\link{summary.Groupwise}}, \code{\link{summary.Conf}}, \code{\link{summary.ConfusionTable}}
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc <- subcascades(predMap,thresh=0.7,numSol=10000)
#' 
#' summary(subc)
summary.Subcascades <- function(object=NULL, includeClassSummary=TRUE,  digits = 3, ...)
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
    size<- strsplit(names(object)[1],'\\.')[[1]][2]
    if(includeClassSummary) {
      ovC <- summaryClasses(object) 
      allOvC <- colSums(ovC)
      nms.allOvC <- names(allOvC)
      allOvC <- matrix(allOvC, nrow=1)
      rownames(allOvC) <- 'all'
      colnames(allOvC) <- nms.allOvC
    }

    cat('-------------------------------------------------------------------------------\n')
    cat('Overall:\n')
    cat(paste('Number of cascades (number): ',sum(ovS[,'number']),'\n', sep = ''))
    cat(paste('Minimal classwise sensitivity (min.class.sens): ',round(mean(ovS[,'min.class.sens']), digits = digits),'\n', sep = ''))
    cat(paste('Maximal cascades length: ',size,'\n', sep = ''))
    if(includeClassSummary) {
      cat(paste('Number of involved classes: ',ncol(allOvC),'\n', sep = ''))
      cat(paste('\nClasses: \n', sep = ''))
      print(colnames(allOvC))
    }
     
    cat('-------------------------------------------------------------------------------\n')
    cat(paste('Sorted by size:\n\n', sep = ''))
         
    print(round(ovS, digits = digits))
    if(includeClassSummary) {
      cat('---------------------------------------------\n')
      cat(paste('Occurrence of classes:\n\n', sep = ''))
      print(allOvC)
      cat(paste('\nOccurrence of classes by size:\n\n', sep = ''))
      print(ovC)
      cat('---------------------------------------------\n')
    }
    
    cat(paste('Longest cascades(size=',size,'):\n\n', sep = ''))
    print(round(object[[1]], digits = digits))
    cat('-------------------------------------------------------------------------------\n')
    return(invisible(object))
}

#' Overview Class Groups
#' 
#' \code{summary.Groupwise} returns a summarizing overview. For each length the number of permutations consisting of the same set of classes is given.
#' 
#' @param object
#' A Groupwise object as it is returned by \code{\link{groupwise}}-function. 
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives an overview of the characteristics of the Groupwise object. 
#' The number of permutations per size is given. A permutation means that the corresponding cascades contain the same classes but with different order.
#' 
#' @seealso \code{\link{groupwise}}, \code{\link{summary.PredictionMap}}, \code{\link{summary.Subcascades}}, \code{\link{summary.Conf}}, \code{\link{summary.ConfusionTable}}
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc = subcascades(predMap,thresh=0.7)
#' groupwise = groupwise(subc,maxCl=50)
#' 
#' summary(groupwise)
summary.Groupwise <- function(object=NULL, ...)
{
  #################################################
  ##
  ## Check parameter 'groupwise'
  
  if(is.null(object))
    return(NULL)
  
  if(!inherits(object, 'Groupwise'))
    stop(errorStrings('groupwise'))
  
  #################################################
  
  maxCl <- object$maxCl
  groupwise <- object$groupings
  
  if(length(groupwise)<1)
    return(NULL)
  
  overview <- sapply(groupwise, function(size){
    
    tab <- table(sapply(size, function(cw){
      if(is.null(cw))
      {
        return(0)
      }else{
        return(nrow(cw))
      }
    }))
    
    res <- rep(0, maxCl*100)
    res[as.numeric(names(tab))] <- as.vector(tab)
    
    return(res)
  })
  
  
  if(!is.matrix(overview))
  {
    overview <- matrix(overview, nrow = maxCl, ncol = length(groupwise))
  }
  
  overview <- t(overview)
  
  colnames(overview) <- paste('perm.',1:ncol(overview),sep = '')
  rownames(overview) <- names(groupwise)
  
  overview <- overview[, colSums(overview)>0,drop=FALSE]
  
  return(overview)
}

#' Summary of prediction maps
#' 
#' \code{summary.PredictionMap} returns a summarizing overview of a PredictionMap object. 
#' 
#' @param object
#' A PredictionMap object as it is returned by \code{\link{predictionMap}}-function. 
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives an overview of the PredictionMap object. 
#' A short summary about the utilized data and labels is given as well as the number of runs and folds of the cross-validation.
#' The summary also includes if the prediction map was generated in parallel and the name of the utilized TunePareto classifier as well as its specified parameters.
#' 
#' @seealso \code{\link{predictionMap}}, \code{\link{summary.Subcascades}}, \code{\link{summary.Groupwise}}, \code{\link{summary.Conf}}, \code{\link{summary.ConfusionTable}}
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' 
#' summary(predMap)
summary.PredictionMap <- function(object=NULL, ...) {
  cat("Object of class PredictionMap\n")
  cat("Prediction map parameters\n\n")
  out.params <- paste(names(object$printParams)," = ",object$printParams,sep="",collapse="\n")
  cat("Parameters: ",out.params, "\n\n")
  
  classifier.params <- paste(object$printClassifierParams,collapse="\n")
  cat("Classifier parameters: ", classifier.params, "\n\n")
}

#' Summary of conf
#' 
#' \code{summary.Conf} returns a summarizing overview of a Conf object. 
#' 
#' @param object
#' A Conf object as it is returned by \code{\link{conf}}-function. 
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives an overview of the Conf object. 
#' The overall number of classes, the highest and lowest sensitivities of the first class as well as the highest and lowest performance of the second class are recorded.
#' 
#' @seealso \code{\link{conf}}, \code{\link{summary.PredictionMap}}, \code{\link{summary.Subcascades}}, \code{\link{summary.Groupwise}}, \code{\link{summary.ConfusionTable}}
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' 
#' conf = conf(predMap)
#' 
#' summary(conf)
summary.Conf <- function(object=NULL, ...) {
  cat("Summary of Conf\n")
  cat("Conf characteristics:\n\n")
  
  fC <- object$fC
  sC <- object$sC
  
  #amount of classes
  nrCl <- sqrt(length(fC))
  cat("Number of classes: ",nrCl,"\n")
  cat("Highest first class sensitivity: ",max(fC),"\n")
  cat("Lowest first class sensitivity: ",min(fC),"\n")
  cat("Highest second class performance: ",max(sC),"\n")
  cat("Lowest second class performance: ",min(sC[sC!=(-1)]),"\n")
}

#' Summary of confusion tables
#' 
#' \code{summary.ConfusionTable} returns a summarizing overview. 
#' 
#' @param object
#' A ConfusionTable object as it is returned by \code{\link{confusionTable}}-function. 
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function gives an overview of the characteristics of the ConfusionTable object. 
#' The cascade with the corresponding mean accuracy are given.
#' 
#' @seealso \code{\link{confusionTable}}, \code{\link{summary.PredictionMap}}, \code{\link{summary.Subcascades}}, \code{\link{summary.Groupwise}}, \code{\link{summary.Conf}}
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
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' 
#' # Calculation of the confusion matrix for '0>2>3>4'.
#' confusionTable = confusionTable(predMap, cascade = '0>2>3>4')
#' 
#' summary(confusionTable)
summary.ConfusionTable <- function(object=NULL, ...) {
  cat("Summary of ConfusionTable\n\n")
  cat("Cascade: ")
  cat(unlist(lapply(strsplit(colnames(object),"cl."),function(x) { x[[2]] })), sep="<")
  cat("\nMean accuracy: ")
  cat(mean(diag(object))*100, "%\n")
}