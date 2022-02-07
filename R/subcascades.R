#' Subcascades Evaluation
#' 
#' \code{Subcascades} returns all cascades found within the data or evaluates a set of specific cascades.
#' 
#' @inheritParams keepSets
#' 
#' @param predictionMap 
#' A PredictionMap object as it is returned by \code{\link{predictionMap}}-function. 
#' It is made up of a list of two matrices(pred and meta). Both matrices provide information on individual samples column-wise.
#' The rownames of the pred-matrix (e.g. [0vs1]) show the classes of the binary base classifier. The elements are the prediction result of a specific training.
#' The rows that correspond to base classifiers that would separate the same class consists of -1. Those rows are not used within the analysis.
#' The meta information connects the values in the pred-matrix to a specific fold, run and contains the original label.
#' 
#' @param thresh
#' A numeric value between 0 and 1.
#' The minimal sensitivity threshold used to filter the returned cascades. 
#' Only cascades that pass this threshold are returned. 
#' If 0 is used the returned cascades are filtered for >0 and otherwise >= thresh.
#' For low thresholds the calculation lasts longer.
#' @param size
#' A numeric value that defines the size of the cascades that should be returned. 
#' The smallest size is 2 and the largest the maximal number of classes of the current dataset.
#' If size is NA (the default setting), all cascades from 2 to the maximal number of classes are evaluated.
#' @param numSol
#' The maximum number of cascades that should pass the first sensitivity bound and are 
#' further evaluated.
#' 
#' @details 
#' This function can either be used to evaluate the performance of a specific cascade, a set of cascades or 
#' to filter out the set of cascades of a specific size and passing a given threshold. 
#' If the sets-variable is given no size can be set. 
#' 
#' @return 
#' A Subcascades object comprising the evaluated cascades and their performances. 
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length and 
#' is sorted row-wise according to the achieved minimal classwise sensitivities of the cascades (decreasing).
#' The rownames show the class order by a character string of type '1>2>3' and the entries the sensitivity for each position of the cascade.
#' 
#' @seealso \code{\link{print.Subcascades}}, \code{\link{plot.Subcascades}}, \code{\link{summary.Subcascades}}
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
#' # change the threshold 
#' # -> returns cascades of all lengths that show a minimal classwise sensitivity >=0.6.
#' subc = subcascades(predMap, thresh=0.6)
#' # search only for cascades of length 2 and 4 
#' # -> returns cascades of length 2 and 4 that show a minimal classwise sensitivity >=0.6.
#' subc = subcascades(predMap, thresh=0.6, size=c(2,4))
#' # evaluates the performance of the cascade '0>1>2>3>4'.
#' subc = subcascades(predMap, sets = c('0>1>2>3>4'))
subcascades<- function(predictionMap=NULL, sets = NULL, thresh=0, size=NA, numSol=1000)
{
    #################################################
    ##
    ## Check parameter 'predictionMap'

    if(is.null(predictionMap))
        stop(errorStrings('predictionMapMissing'))
    
    if(!inherits(predictionMap, 'PredictionMap'))
        stop(errorStrings('predictionMap'))

    #################################################
        
    labs      <- predictionMap$meta['label',]
    classes   <- sort(unique(labs))
    
    #################################################
    ##
    ## Check parameter sets and thresh and size

    if (is.null(sets)){
      ##
      ## Check parameter 'thresh'
  
      if(!is.numeric(thresh) | length(thresh)!=1)
          stop(errorStrings('thresh'))
      
      if(thresh<0 | thresh>1 )
          stop(errorStrings('thresh'))
      
      ##
      ## Check parameter 'size'
      
      numClass <- length(classes)
      
      if(is.na(size[1]) & length(size)==1)
          size <- numClass:2
    
      if(!is.numeric(size)|any(is.na(size)))
          stop(errorStrings('size.na'))
   
      if(any(size<2)|any(size>numClass))
          stop(errorStrings('size.cl'))
      
    } else { #check parameter sets
      
      if((is.character(sets) & is.vector(sets))){
        correct <- regexpr(pattern = '^([[:digit:]]+>)+[[:digit:]]+$', text = sets)>0
        
        if(!correct)
            stop(errorStrings('sets.cascades'))
        
        sets      <- lapply(sets, function(x){as.numeric(strsplit(x,'>',fixed=T)[[1]])})
      }
      
      if(is.numeric(sets) & is.vector(sets))
        sets <- list(sets)
      
      if(!is.list(sets))
      {
        stop(errorStrings('sets'))
      }else{
        if(!all(sapply(sets, function(x){is.numeric(x)&is.vector(x)})))
          stop(errorStrings('sets'))
      }
      
      if(!all(unique(unlist(sets)) %in% classes))
          stop(errorStrings('wrong.classes'))
      
      if (!is.na(size))
          stop(errorStrings('noSize'))
    }
      
    #################################################      
    ##
    ## Check parameter 'numSol'
 
    if(!is.numeric(numSol) | length(numSol)!=1)
        stop(errorStrings('numSol'))
    
    if(numSol<1)
        stop(errorStrings('numSol'))

    #################################################

    conf      <- conf(predictionMap)
    
    if (!is.null(sets)){#sets
      set.sizes = sapply(sets,length)
      
      sizes <- sort(unique(set.sizes), decreasing = TRUE)
      
      cand <- lapply(sizes, function(i){
          mat <- matrix(unlist(sets[set.sizes==i]), ncol = i, byrow = TRUE)
          colnames(mat) = paste('pos.',seq(i),sep='')
          rownames(mat) = paste('size.',i,'_',seq(nrow(mat)),sep='')
          return(mat)
      })
      names(cand) <- paste('size.',sizes,sep='')
    }else{#all
      size      <- sort(unique(size), decreasing=T)
      cand      <- cand.subcascades(conf, thresh=thresh, size=size, numSol=numSol)
    }
    
    subc <- lapply(cand, function(x){
        casc <- extract.classwise.sensitivities(predictionMap, x)
        
        if(is.null(casc))
            return(NULL)
        
        min.sens <- sort(apply(casc,1,min), decreasing=TRUE)
        
        if (thresh == 0 & is.null(sets)){
          keep <- min.sens > thresh
        }else{
          keep <- min.sens >= thresh
        }
        
        
        if(sum(keep)==0)
        {
            return(NULL)
        }else{
            return(casc[names(min.sens)[keep],,drop = FALSE])
        }
    })
    
    subc <- subc[sapply(subc, function(x){!is.null(x)})]
    
    if(length(subc)==0)
    {
        return(NULL)
    }else{
        structure(subc,class = "Subcascades")
    }
}

#' Merge Subcascades
#' 
#' \code{mergeSubcascades} adds the cascades from subcascades2 to the subcascades1, that have not been part of subcascades1.
#' 
#' @param subcascades1
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function.
#' The Subcascades object is made up of a list of matrices. 
#' Each matrix comprises the evaluation results of cascades of a specific length. 
#' The rownames show the class order and the entries the sensitivity for each position of the cascade.
#' @param subcascades2
#' A Subcascades object like subcascades1
#' 
#' @inherit subcascades return
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
#' # make two Subcascades objects
#' subc1 = subcascades(predMap, size = c(3,4), thresh = 0.6)
#' subc2 = subcascades(predMap, size = c(4), thresh = 0.5)
#' # add the cascades of subcascades2 to subcascades1
#' mergeSubcascades(subc1, subc2)
mergeSubcascades <- function(subcascades1=NULL, subcascades2=NULL){
  #################################################
  ##
  ## Check parameter 'subcascades1'
  
  if(!is.null(subcascades1) & !inherits(subcascades1, 'Subcascades'))
    stop(errorStrings('subcascades1'))
  
  #################################################
  ##
  ## Check parameter 'subcascades2'
  
  if(!is.null(subcascades2) & !inherits(subcascades2, 'Subcascades'))
    stop(errorStrings('subcascades2'))
  
  #################################################
  
  if(!is.null(subcascades1))
    subcascades1 <- subcascades1[sapply(subcascades1, function(x){!is.null(x)})]
  
  if(!is.null(subcascades2))
    subcascades2 <- subcascades2[sapply(subcascades2, function(x){!is.null(x)})]
  
  if(length(subcascades1)!=0)
  {
    result = subcascades1
    
    if(length(subcascades2)!=0)
    {
      for (size in names(subcascades2)){
        if (size %in% names(result)){
          add.casc = !(rownames(subcascades2[[size]]) %in% rownames(result[[size]]))
          result[[size]] = rbind(result[[size]],subcascades2[[size]][add.casc,])
          Smin = apply(result[[size]],1,min)
          result[[size]][order(Smin,decreasing=T),]
        } else {
          result[[size]] = subcascades2[[size]]
        }
      }
      
      size.as.numeric <- as.numeric(sapply(names(result), function(x){strsplit(x,'.',fixed = TRUE)[[1]][2]}))
      result = result[order(size.as.numeric,decreasing = T)]
    }
  }else{
    result = subcascades2
  }
  
  structure(result,class='Subcascades')
}

#' Coerce to a Subcascades Object 
#' 
#' Converts from a Groupwise object to a Subcascades object.
#' 
#' @param groupwise
#' A Groupwise object, which comprises a two-leveled list. The first level collects cascades of the same size.
#' The second level contains a list of unique class combinations, labelled as a character string with '-' separating the different classes. 
#' For each unique set of class combinations the corresponding orders and their performance are given as a matrix, 
#' where each row contains a cascade, given as a character string of type '1>2>3', and the columns the sensitivity for the class at the corresponding position.
#' Each matrix is sorted row-wise according to the achieved minimal classwise sensitivites of the cascades (decreasing).
#' 
#' @details 
#' Converts a Groupwise object to a Subcascades object.
#' 
#' @inherit subcascades return
#' 
#' @seealso \code{\link{groupwise}}, \code{\link{as.edgedataframe}}
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
#' # generate a Groupwise object
#' subc = subcascades(predMap,thresh=0.7)
#' groupwise = groupwise(subc)
#' 
#' #convert it to a Subcascades object
#' converted.subcascades = as.subcascades(groupwise)
as.subcascades <- function(groupwise=NULL)
{
  #################################################
  ##
  ## Check parameter 'groupwise'
  
  if(is.null(groupwise))
    return(NULL)
  
  if(!inherits(groupwise, 'Groupwise'))
    stop(errorStrings('groupwise'))
  
  #################################################
  
  groupwise <- groupwise$groupings
  groupwise <- groupwise[sapply(groupwise, function(x){!is.null(x)})]
  
  subcascades <- lapply(groupwise, function(size){
    casc <- do.call('rbind', size)
    min.sens <- sort(apply(casc,1,min), decreasing=TRUE)
    return(casc[names(min.sens),,drop = FALSE])
  })
  
  if(length(subcascades)==0)
  {
    return(NULL)
  }else{
    structure(subcascades,class='Subcascades')
  }
}

#generic function for formatting outputs of a Subcascades object
format <- function(subcascades, ...) UseMethod("format")

#implementation of the generic function \code{\link{format}} to give an formatted output of a Subcascades output
format.Subcascades <- function(x, printSizes=length(x), ...) {
  #if user specified a size too large, change to length of subcascades list
  if(printSizes > length(x)) {
    printSizes <- length(x)
  }
  print.default(x[1:printSizes],...)
}