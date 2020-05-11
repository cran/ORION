## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = '#>')
oldoptions <- options()
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(ORION)
library(TunePareto)
library(graphics)
library(igraph)
set.seed(1014)

## -----------------------------------------------------------------------------
# Load and evaluate the dataset
data(esl_org)
#names of the features
colnames(esl_org)
#dimensions of the dataset
dim(esl_org)

## -----------------------------------------------------------------------------
# Extract and define class labels
labels = esl_org[,5]-1
table(labels)
#maybe merge subgroups
labels[labels<=2] = 2
labels[labels>=6] = 6
table(labels)

## -----------------------------------------------------------------------------
#redefine the labels such that they start at 0 and are consecutive
labels = labels-2
table(labels)

## -----------------------------------------------------------------------------
data = esl_org[,1:4]

## -----------------------------------------------------------------------------
#generate a fold list
library(TunePareto)
foldList = generateCVRuns(  labels= labels,
                            ntimes      = 10,
                            nfold       = 10,
                            leaveOneOut = FALSE,
                            stratified  = TRUE)

## -----------------------------------------------------------------------------
#generate the predicition map
genMap = gen.predictionMap(data, labels, foldList = foldList, 
                            classifier = tunePareto.svm(), kernel='linear')

## -----------------------------------------------------------------------------
names(genMap)

## -----------------------------------------------------------------------------
genMap$meta[,c(1,11,21)]

## -----------------------------------------------------------------------------
genMap$pred[1:5,c(1,11,21)]

## -----------------------------------------------------------------------------
#analyse the subcascades
subcascades = subcascades(genMap, thresh=0.6)
print(subcascades, max = 10)

## -----------------------------------------------------------------------------
# create a trained classifier model for to top ranked cascade of length 5
model <- trainTuneParetoClassifier( 
          classifier  = tunePareto.occ( base.classifier = tunePareto.svm()),
          trainData   = data,
          trainLabels = labels,
          class.order = as.character(c(0,1,2,3,4)),
          kernel      = 'linear',
          cost        = 1)
          
# predict labels
prediction <- predict(object = model, newdata = data)
# calculate the class-wise sensitivities
sensitivites = table(prediction[prediction==labels])/table(labels)

## -----------------------------------------------------------------------------
summary(subcascades)

## -----------------------------------------------------------------------------
mat <- summaryGroupwise(subcascades)
mat

## -----------------------------------------------------------------------------
groupwise <- as.groupwise(subcascades)
groupwise$size.4

## -----------------------------------------------------------------------------
subcascades.rev <- as.subcascades(groupwise)

## -----------------------------------------------------------------------------
#subcascades passing the threshold of 0.7
result <- keepThreshold(subcascades, thresh = 0.7)
#the minimal sensitivity of all filtered cascade is > 0.7
names(result)
apply(result$size.4,1,min)

## -----------------------------------------------------------------------------
#subcascades that do not show a minimal class-wise sensitivity higher than 0.70
result <- dropThreshold(subcascades, thresh = 0.7)
#the minimal sensitivity of all filtered cascade is < 0.7
apply(result$size.4,1,min)

## -----------------------------------------------------------------------------
#keep all cascades of length 3
result <-keepSize(subcascades, size = 4)
#only size.4 is returned
names(result)

## -----------------------------------------------------------------------------
#keep all cascades except those of length 3
result <-dropSize(subcascades, size = 3)
#size.3 is not within the set anymore
names(result)

## -----------------------------------------------------------------------------
result <- keepSets(subcascades, sets = list(c(0,1,2),c(2,3,4)), 
                    direction = 'exact', ordered=F)
unlist(lapply(result,rownames))
result <- keepSets(subcascades, sets = c(0,1,2),
                    direction = 'exact',ordered=T)
unlist(lapply(result,rownames))
result <- keepSets(subcascades, sets = c('0>1>2','2>3>4'), 
                    direction = 'exact',ordered=T)
unlist(lapply(result,rownames))

## -----------------------------------------------------------------------------
#cascades can be given either as list of numeric vectors or as a vector of character strings
set1 = list(c(0,1,2),c(2,3,4))
set2 = c('0>1>2','2>3>4')

## -----------------------------------------------------------------------------
result <- keepSets(subcascades, sets=set1, direction = 'super', 
                    ordered = FALSE, neighborhood = 'direct', type = 'any')
result

## -----------------------------------------------------------------------------
result.all <- keepSets(subcascades, sets=set1, direction = 'super', 
                        ordered = FALSE, neighborhood = 'direct', type = 'all')
unlist(t(lapply(result.all,rownames)))

## -----------------------------------------------------------------------------
result <- keepSets(subcascades, sets=set1, direction = 'super', 
                    ordered = FALSE, neighborhood = 'direct', type = 'any')
unlist(t(lapply(result,rownames)))
result <- keepSets(subcascades, sets=set1, direction = 'super', 
                    ordered = FALSE, neighborhood = 'indirect', type = 'any')
unlist(t(lapply(result,rownames)))

## -----------------------------------------------------------------------------
result <- dropSets(subcascades, sets=set1, direction = 'super', 
                    ordered = FALSE, neighborhood = 'direct', type = 'any')
result$size.4

## -----------------------------------------------------------------------------
result.neighbourhood = confusion.table(genMap, cascade = '0>1>3>4', 
                                        other.classes='all', sort = TRUE)
result.neighbourhood

## ----fig.width=5, fig.height=6, fig.align = 'center', strip.white=TRUE,echo = TRUE----
subcascades = dropSize(subcascades,c(2,3))
plot(subcascades,row.sort = 'max',digits=2,thresh = 0.6)

## ----fig.width=5, fig.asp = 0.7, fig.align = 'center', strip.white=TRUE,echo = FALSE----
plot(result.neighbourhood, digits=2)

## -----------------------------------------------------------------------------
base.classifier = gen.conf(genMap)

## ----fig.width=5, fig.asp = 0.9, fig.align = 'center', strip.white=TRUE,echo = FALSE----
plot(base.classifier, digits=2,symmetric=TRUE, na.color = 'white')

## -----------------------------------------------------------------------------
#filter for minimal class-wise sensitivity and size
result = keepThreshold(subcascades,0.7)
result = keepSize(result,c(4,5))
#convert to a dataframe that can be used in igraph
edges = as.edgedataframe(result)
#use the first and second column to make a graph object
g = graph_from_data_frame(edges[,c(1,2)], directed = TRUE)
#assign the subcascade IDs as edge weights
E(g)$weight = edges[,3]

## ----fig.width=5, fig.asp = 0.9, fig.align = 'center', strip.white=TRUE,echo = FALSE----
plot(g,edge.color=E(g)$weight,edge.arrow.size=0.5,
     edge.curved =seq(-0.5, 1, length = ecount(g)))
options(oldoptions)

