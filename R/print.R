#' Prints all details about a PredictionMap object.
#' 
#' @param x
#' A PredictionMap object generated by \code{\link{predictionMap}}.
#' 
#' @param showMeta
#' If TRUE the meta data of the PredictionMap object will be printed.
#' 
#' @param showPred
#' If TRUE the predictions of the PredictionMap object will be printed.
#' 
#' @param ...
#' Further parameters
#' 
#' @details 
#' Prints all details about a given PredictionMap object and information about the meta information as well as the predictions.
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
#'                           
#' # svm with linear kernel
#' predMap = predictionMap(data, labels, foldList = foldList, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#'                            
#' print(predMap, showMeta=TRUE, showPred=TRUE)
print.PredictionMap <- function(x, showMeta=TRUE, showPred=TRUE , ...) {
  if(!inherits(x, "PredictionMap"))
    stop("\"x\" must be a PredictionMap object!")
  
  format.PredictionMap(x, showMeta=showMeta, showPred=showPred, ...)
  return(invisible(x))
}

#' Prints all details about a Subcascades object.
#' 
#' @param x
#' A Subcascades object generated by \code{\link{subcascades}}.
#' 
#' @param printSizes
#' Integer specifying how many elements of a Subcascades object should be printed, 
#' sorted decreasingly after cascade length. 
#' 
#' @param ...
#' Further parameters
#' 
#' @details 
#' Prints all details about a given Subcascades object.
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
#'                        classifier = tunePareto.svm(),  kernel='linear')
#' 
#' # use default parameter settings 
#' # -> returns cascades of all lengths that show a minimal classwise sensitivity >0.
#' subc = subcascades(predMap)
#' 
#' # print subcascades for the largest 2 sizes
#' print(subc, printSize=2)
print.Subcascades <- function(x, printSizes=length(x), ...) {
  if(!inherits(x, "Subcascades"))
    stop("\"x\" must be a Subcascades object!")
  
  format.Subcascades(x, printSizes=printSizes, ...)
  return(invisible(x))
}

#' Prints all details about a Conf object.
#' 
#' @param x
#' A Conf object as it is returned by \code{\link{conf}}-function. 
#' 
#' @param printfC
#' If TRUE, the sensitivities of the first classes are printed (TP = true positives).
#' 
#' @param printsC
#' If TRUE, the values of sC are printed (TN = true negatives). The values of sC 
#' correspond to the rate of samples of a given class (column), which are assigned to the 
#' second class of the base classifier (row).
#'
#' @param ...
#' Further parameters
#' 
#' @details 
#' Prints all details about a given Conf object. 
#' A Conf object provides the conditional prediction rates of all base classifiers applied to all classes.
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
#' print(conf)
print.Conf <- function(x, printfC=TRUE, printsC=TRUE, ...) {
  if(!inherits(x, "Conf"))
    stop("\"x\" must be a Conf object!")
  
  format.Conf(x, printfC=printfC, printsC=printsC, ...)
  return(invisible(x))
}

#' Prints all details about a ConfusionTable object.
#' 
#' @param x
#' A ConfusionTable object as it is returned by \code{\link{confusionTable}}-function. 
#' 
#' @param ...
#' Further parameters
#' 
#' @details 
#' Prints all conditional prediction rates for the specified cascade.
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
#' confTable = confusionTable(predMap, cascade = '0>2>3>4')
#' 
#' print(confTable)
print.ConfusionTable <- function(x, ...) {
  if(!inherits(x, "ConfusionTable"))
    stop("\"x\" must be a ConfusionTable object!")
  
  format.ConfusionTable(x, ...)
  return(invisible(x))
}

#' Prints all details about a Groupwise object.
#' 
#' @param x
#' A Groupwise object as it is returned by \code{\link{groupwise}}-function. 
#' 
#' @param printSizes
#' Integer that specifies how many of the cascade sizes (starting with the largest cascades) 
#' should be printed.
#' 
#' @param ...
#' Further parameters
#' 
#' @details 
#' Prints all details about a given Groupwise object. 
#' A Groupwise object re-sorts a Subcascades object 
#' in a way that the cascades made up of the same classes are grouped. 
#' The printSizes parameter can be used to control the cascade sizes to be printed.
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
#' # generate Subcascades object
#' subc = subcascades(predMap,thresh=0.7)
#' 
#' #create a Groupwise object
#' groupwise = groupwise(subc)
#' 
#' print(groupwise, printSizes = 2)
print.Groupwise <- function(x, printSizes = length(x), ...) {
  if(!inherits(x, "Groupwise"))
    stop("\"x\" must be a Groupwise object!")
  
  format.Groupwise(x, printSizes = printSizes, ...)
  return(invisible(x))
}