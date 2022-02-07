train.tunePareto.occ <- function(..., data, labels, base.classifier, class.order = NULL)
{
  ##################################################################
  #### Checks
  
  if (missing(data) || missing(labels) || missing(base.classifier) || missing(class.order))
    stop('Please supply at least \'data\', \'labels\', \'base.classifier\', \'class.order\'!')
  
  if (!inherits(base.classifier, 'TuneParetoClassifier'))
    stop('\'base.classifier\' must be a TuneParetoClassifier object!')
  
  if (length(labels) != nrow(data))
    stop('Dimensions of data matrix and class label vector are incompatible!')
  
  ####
  if (!is.vector(class.order))
    stop('class.order is required to be a vector.')
  
  if(is.character(class.order) & length(class.order)==1){
    class.order = as.character(strsplit(class.order,'>',fixed=T)[[1]])
  }
  
  if(is.numeric(class.order)){
    class.order = as.character(class.order)
  }
  ####
  
  if (!all(class.order %in% labels))
    stop('Unknown classes detected in class order.')
  
  
  args <- list(...)
  # 
  
  nonmatch <- setdiff(names(args), c(base.classifier$classifierParamNames, base.classifier$predictorParamNames))
  
  if (length(nonmatch) > 0)
    stop('The following unknown parameters have been specified: ', nonmatch)
  
  parameters = TunePareto::allCombinations(args)
  
  alternatives <- sapply(args, function(param) length(param) > 1)
  
  if(any(alternatives))
    stop('Only unique parameters are allowed for base.classifier:')
  # 
  ####################################################################
  ####
  #### Routine
  
  base.classifiers <- lapply(1:(length(class.order)-1), function(i){
    
    index <- c(which(labels==class.order[i]),which(labels==class.order[i+1]))
    
    TunePareto::trainTuneParetoClassifier(  classifier = base.classifier,
                                            trainData  = data[index,,drop = FALSE],
                                            trainLabels= labels[index],
                                            ...)
  })
  
  result <- list( base.classifiers = base.classifiers,
                  class.order = class.order,
                  labels = labels)
  
  ##################################################################
  ####
  #### Result
  
  return(result)
}

predict.tunePareto.occ <- function(object, newdata,...)
{
  ##################################################################
  #### Checks
  
  if (missing(newdata))
    stop('Please supply \'newdata\' !')
  
  ####################################################################
  ####
  #### Routine
  
  base.predictions <- sapply(object$base.classifiers, function(bC){predict(bC,newdata,...)})
  
  if(!is.matrix(base.predictions))
    base.predictions <- matrix( base.predictions,
                                nrow = nrow(newdata),
                                ncol = length(object$base.classifiers))
  
  numClasses <- length(object$base.classifiers)+1
  
  ensemble.predictions <- apply(base.predictions,1,function(x){
    index <- which(x==object$class.order[-numClasses])
    
    if(length(index)==0)
    {
      object$class.order[numClasses]
    }else{
      object$class.order[index[1]]
    }
  })
  
  ##################################################################
  ####
  #### Result
  
  ensemble.predictions <- factor(x=as.numeric(ensemble.predictions),
                                 levels=sort(unique(object$labels)))
  
  return(ensemble.predictions)
}

#' Ordinal Classifier Cascade Tune Pareto Object
#' 
#' TunePareto wrapper for the ordinal classifier cascade.
#' 
#' @param base.classifier
#' A predefined TuneParetoClassifier object used as binary classifier 
#' for the ordinal classifier cascade. There exist five classifier types that 
#' can be used: tunePareto.knn(), tunePareto.svm(), tunePareto.tree(), tunePareto.randomForest(),
#' tunePareto.NaiveBayes(). For more information about these classifier functions please refer to 
#' the corresponding help page of like \code{TunePareto::\link{tunePareto.knn}}.
#' 
#' @details 
#' The "tunePareto.occ" encapsulates the classifier of an ordinal classifier cascade.
#' Additionally, to the parameters of the corresponding base classifier the "class.order"
#' parameter can be provided. It is either a character vector, a numeric vector, 
#' or a vector representing a cascade of the following format '1>2>4'.
#' 
#' @return 
#' Returns an object of class TuneParetoClassifier (see: \code{TunePareto::\link{tuneParetoClassifier}}). 
#' This can be passed to the function trainTuneParetoClassifier (see: \code{TunePareto::\link{trainTuneParetoClassifier}}).
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data = esl$data
#' labels = esl$labels
#' # train classifier
#' model <- trainTuneParetoClassifier( 
#'          classifier  = tunePareto.occ( base.classifier = tunePareto.svm()),
#'          trainData   = data,
#'          trainLabels = labels,
#'          class.order = as.character(c(4,3,1,0)),
#'          kernel      = "linear",
#'          cost        = 1)
#'          
#' # predict labels
#'prediction <- predict(object = model, newdata = data)
tunePareto.occ <- function(base.classifier)
{
    ##################################################################
    ####
    #### Checks 
    
    if ( missing(base.classifier))
        stop(errorStrings("classifierMissing"))
    
    if (!inherits(base.classifier, "TuneParetoClassifier"))
        stop(errorStrings("classifier"))
    
    ####################################################################
    ####
    #### Routine
    
    result <- TunePareto::tuneParetoClassifier( name                        = paste("occ(",base.classifier$name,")", sep = ""),
                                                classifier                  = train.tunePareto.occ,
                                                classifierParamNames        = c("base.classifier","class.order",base.classifier$classifierParamNames),
                                                predefinedClassifierParams  = c(list(base.classifier=base.classifier),base.classifier$predefinedClassifierParams),
                                                predictor                   = predict.tunePareto.occ,
                                                predictorParamNames         = base.classifier$predictorParamNames,
                                                predefinedPredictorParams   = base.classifier$predefinedPredictorParams,
                                                trainDataName               = "data",
                                                trainLabelName              = "labels",
                                                testDataName                = "newdata",
                                                modelName                   = "object",
                                                requiredPackages            = base.classifier$requiredPackages)
    
    
    ##################################################################
    ####
    #### Result
    
    return(result)
}

