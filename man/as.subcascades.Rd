% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subcascades.R
\name{as.subcascades}
\alias{as.subcascades}
\title{Coerce to a Subcascades Object}
\usage{
as.subcascades(groupwise = NULL)
}
\arguments{
\item{groupwise}{A Groupwise object, which comprises a two-leveled list. The first level collects cascades of the same size.
The second level contains a list of unique class combinations, labelled as a character string with '-' separating the different classes. 
For each unique set of class combinations the corresponding orders and their performance are given as a matrix, 
where each row contains a cascade, given as a character string of type '1>2>3', and the columns the sensitivity for the class at the corresponding position.
Each matrix is sorted row-wise according to the achieved minimal classwise sensitivites of the cascades (decreasing).}
}
\value{
A Subcascades object comprising the evaluated cascades and their performances. 
The Subcascades object is made up of a list of matrices. 
Each matrix comprises the evaluation results of cascades of a specific length and 
is sorted row-wise according to the achieved minimal classwise sensitivities of the cascades (decreasing).
The rownames show the class order by a character string of type '1>2>3' and the entries the sensitivity for each position of the cascade.
}
\description{
Converts from a Groupwise object to a Subcascades object.
}
\details{
Converts a Groupwise object to a Subcascades object.
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

# generate a Groupwise object
subc = subcascades(predMap,thresh=0.7)
groupwise = groupwise(subc)

#convert it to a Subcascades object
converted.subcascades = as.subcascades(groupwise)
}
\seealso{
\code{\link{groupwise}}, \code{\link{as.edgedataframe}}
}
