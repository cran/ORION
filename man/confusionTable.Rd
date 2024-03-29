% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusionTable.R
\name{confusionTable}
\alias{confusionTable}
\title{Construction of a Confusion Table}
\usage{
confusionTable(
  predictionMap = NULL,
  cascade = NULL,
  other.classes = NULL,
  sort = TRUE
)
}
\arguments{
\item{predictionMap}{A PredictionMap object as it is returned by \code{\link{predictionMap}}-function. 
It is made up of a list of two matrices(pred and meta). Both matrices provide information on individual samples column-wise.
The rownames of the pred-matrix (e.g. [0vs1]) show the classes of the binary base classifier. The elements are the prediction result of a specific training.
The rows that correspond to base classifiers that would separate the same class consists of -1. Those rows are not used within the analysis.
The meta information connects the values in the pred-matrix to a specific fold, run and contains the original label.}

\item{cascade}{A numeric vector of classes or a character string of type '1>2>3' of at least two class labels reflected in 'predictionMap'.}

\item{other.classes}{This parameter can be either NULL, 'all' or a numeric vector of classes that are not part of the cascade parameter.
If other.classes is:
- NULL, only the cascade classes are evaluated. 
- 'all', all remaining classes are evaluated. 
-  a vector of classes, those classes are evaluated.}

\item{sort}{If TRUE (default) the classes that are not part of cascade are sorted based on their confusion.}
}
\value{
An object of type ConfusionTable including the sensitivities, with the label of the predicted classes in the rows and the labels of the original class in the columns.
}
\description{
Confusion table and class assignments of one cascade.
}
\examples{
library(TunePareto)
data(esl)
data = esl$data
labels = esl$labels
foldList = generateCVRuns(labels  = labels,
                          ntimes      = 2,
                          nfold       = 2,
                          leaveOneOut = FALSE,
                          stratified  = TRUE)
predMap = predictionMap(data, labels, foldList = foldList, 
                       classifier = tunePareto.svm(), kernel='linear')

# Calculation of the confusion matrix for '0>2>3>4'.
confTable = confusionTable(predMap, cascade = '0>2>3>4')
# Calculation of the confusion matrix for '0>2>3>4' 
# and the assignment of all samples of the other classes.
confTable = confusionTable(predMap, cascade = '0>2>3>4', 
                other.classes='all', sort = TRUE)
}
\seealso{
\code{\link{summary.ConfusionTable}}, \code{\link{print.ConfusionTable}}, \code{\link{plot.ConfusionTable}}
}
