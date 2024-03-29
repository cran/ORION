% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictionMap.R
\name{predictionMap}
\alias{predictionMap}
\title{Construction of a Prediction Map Object}
\usage{
predictionMap(
  data = NULL,
  labels = NULL,
  foldList = NULL,
  parallel = FALSE,
  classifier = NULL,
  ...
)
}
\arguments{
\item{data}{The data either as matrix or data.frame with samples in rows and features in columns.}

\item{labels}{A vector of labels of data, consecutively labelled starting with 0.}

\item{foldList}{A set of partitions for a cross-validation experiment. 
This list comprises as many elements as cross-validation runs. 
Each run is a list of as many vectors as folds. The entries are the indices of the samples that are left out in the folds.
This fold list can be generated by using \code{TunePareto::\link{generateCVRuns}}.
If the foldList is set to \code{NULL} (default) reclassification is performed.}

\item{parallel}{Either TRUE or FALSE (default). If TRUE the pairwise training is performed parallelized.}

\item{classifier}{A TunePareto classifier object. \cr 
For detailed information refer to \code{TunePareto::\link{tuneParetoClassifier}}.}

\item{...}{Further parameters of the classifier object.}
}
\value{
A PredictionMap object.
It is made up of a list of two matrices, which are called meta and pred. Both matrices provide information for individual samples column-wise. The meta information in meta connects the values in the pred-matrix to a specific fold, run, sample and contains the original label.
The rownames of the pred-matrix (e.g. [0vs1]) show the classes of the binary base classifier. The elements are the prediction result of a specific training.
The rows that correspond to base classifiers that would separate the same class consists of -1. Those rows are not used within the analysis.
}
\description{
Makes a PredictionMap object for the given data.
}
\details{
Using a reclassification or a cross-validation set up this function performs a pairwise training for all class combinations and evaluates all samples using the trained classifiers.
This means that for each run, fold and binary classifier the predicted class for each sample is calculated.
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
                          
# svm with linear kernel
predMap = predictionMap(data, labels, foldList = foldList, 
                       classifier = tunePareto.svm(), kernel='linear')
\donttest{
# knn with k = 3
predMap = predictionMap(data, labels, foldList = foldList, 
                       classifier = tunePareto.knn(), k = 3)
# randomForest
predMap = predictionMap(data, labels, foldList = foldList, 
                       classifier = tunePareto.randomForest())
                           }
}
\seealso{
\code{\link{summary.PredictionMap}}, \code{\link{print.PredictionMap}}, \code{\link{plot.PredictionMap}}
}
