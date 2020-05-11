#' Subcascades Evaluation
#' 
#' \code{Subcascades} returns all cascades found within the data or evaluates a set of specific cascades.
#' 
#' @inheritParams keepSets
#' 
#' @param predictionMap 
#' A PredictionMap object as it is returned by \code{\link{gen.predictionMap}}-function. 
#' It is made up of two elements(pred and meta).
#' The rownames of the pred-matrix (e.g. \[0vs1\]) show the classes of the binary base classifier. The elements are the prediction result of a specific training.
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
#' @seealso \code{\link{plot.Subcascades}}, \code{\link{summarySubcascades}}
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
#' genMap = gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(),  kernel='linear')
#' 
#' # use default parameter settings 
#' # -> returns cascades of all lengths that show a minimal classwise sensitivity >0.
#' subcascades = subcascades(genMap)
#' # change the threshold 
#' # -> returns cascades of all lengths that show a minimal classwise sensitivity >=0.6.
#' subcascades = subcascades(genMap, thresh=0.6)
#' # search only for cascades of length 2 and 4 
#' # -> returns cascades of length 2 and 4 that show a minimal classwise sensitivity >=0.6.
#' subcascades = subcascades(genMap, thresh=0.6, size=c(2,4))
#' # evaluates the performance of the cascade '0>1>2>3>4'.
#' subcascades = subcascades(genMap, sets = c('0>1>2>3>4'))


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

    conf      <- gen.conf(predictionMap)
    
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
        class(subc) <- 'Subcascades'
        return(subc)
    }
}

