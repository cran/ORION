# Internal: selection of the learner classifier and prediction wrapper
############################################################
############################################################
.train_classifier <- function(
    learner,
    trainData,
    trainLabels,
    classifier = NULL,
    ...){
  switch(learner,
         tunePareto = {
           if(is.null(classifier))
             stop("'classifier' must be provided for TunePareto learner.")
           
           TunePareto::trainTuneParetoClassifier(
             classifier = classifier,
             trainData = trainData,
             trainLabels = trainLabels,
             ...)
         },
         e1071 = {
           e1071::svm(
             x = trainData,
             y = trainLabels,
             ...)
         },
         stop("Unknown or unsupported learner:", learner)
  )  
}


############################################################
############################################################
.predict_classifier <- function(model, newdata) {
  as.numeric(as.character(
    predict(model, newdata = newdata)
  ))
}