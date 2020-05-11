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
#' parameter can be provided.
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

