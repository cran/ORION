#' Filters for Threshold
#' 
#' Filters for all cascades that match the comparison with a minimal classwise sensitivity threshold.
#' 
#' @inheritParams subcascades
#' @inheritParams summarySubcascades
#' @param comparison
#' Defines the comparison type (<,>,<=,>=) for the threshold.
#' 
#' 
#' @inherit subcascades return
#' 
#' @seealso \code{\link{dropSize}}, \code{\link{keepSize}}, \code{\link{dropSets}}, \code{\link{keepSets}}, \code{\link{dropThreshold}}
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
#' # generate Subcascades object
#' subc = subcascades(predMap,thresh=0.5)
#' 
#' # filters for cascades that 
#' # 1. have a minimal classwise sensitivity >= 0.6
#' keepThreshold(subc,thresh=0.6)
#' # 2. have a minimal classwise sensitivity <= 0.6
#' keepThreshold(subc, comparison = '<=', thresh=0.6)


keepThreshold <- function(subcascades = NULL, thresh=0, comparison = '>=')
{
    #################################################
    ##
    ## Check parameter 'subcascades'
    
    if(is.null(subcascades))
        return(NULL)
    
    if(!inherits(subcascades, 'Subcascades'))
        stop(errorStrings('subcascades'))
    
    #################################################
    ##
    ## Check parameter 'comparison'
    
    if(!is.character(comparison) | length(comparison)!=1)
        stop(errorStrings('comparison'))
    
    if(!(comparison %in% c('>=','>','<=','<','==','!=')))
       stop(errorStrings('comparison'))
    
    #################################################
    ##
    ## Check parameter 'thresh'
    
    if(!is.numeric(thresh) | length(thresh)!=1)
        stop(errorStrings('thresh'))
        
    if(thresh<0 | thresh>1 )
        stop(errorStrings('thresh'))
    
    #################################################

    subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
    {
        return(NULL)
    }
    
    subcascades <- lapply(subcascades, function(casc){
        
        min.class.sens <- apply(casc,1,min)
        keep <- switch(comparison,
                    '>=' = min.class.sens>=thresh,
                    '>' = min.class.sens>thresh,
                    '<=' = min.class.sens<=thresh,
                    '<' = min.class.sens<thresh,
                    '==' = min.class.sens==thresh,
                    '!=' = min.class.sens!=thresh)
        
        if(sum(keep)==0)
        {
            return(NULL)
        }else{
            casc[keep,,drop=FALSE]
        }
    })
    
    subcascades <- subcascades[sapply(subcascades, function(x){!is.null(x)})]
    
    if(length(subcascades)==0)
    {
        return(NULL)
    }else{
        class(subcascades) <- 'Subcascades'
        return(subcascades)
    }
}
