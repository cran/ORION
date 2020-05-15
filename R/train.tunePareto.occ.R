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
  
  if (!is.character(class.order))
    stop(' class.order is required to be a character vector.')
  
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
  #### Rountine
  
  base.classifiers <- lapply(1:(length(class.order)-1), function(i){
    
    index <- c(which(labels==class.order[i]),which(labels==class.order[i+1]))
    
    TunePareto::trainTuneParetoClassifier(  classifier = base.classifier,
                                            trainData  = data[index,,drop = FALSE],
                                            trainLabels= labels[index],
                                            ...)
  })
  
  result <- list( base.classifiers = base.classifiers,
                  class.order = class.order)
  
  ##################################################################
  ####
  #### Result
  
  return(result)
}