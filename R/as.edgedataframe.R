#' Coerce to an Edge List
#' 
#' Converts from a Subcascades object to a weighted edge list.
#' 
#' @inheritParams summarySubcascades
#' 
#' @details 
#' Converts a Subcascades object to a data.frame that can be used to generate a graph. 
#' 
#' @inherit subcascades
#' 
#' @return A data.frame that can be used to generate a graph. 
#' The first and second column correspond to all pairwise relations (from - to) of the cascades within the
#' Subcascades object. The 'CASC_ID' column contains the same ID for all edges belonging to the same cascade.
#' The 'SIZE' column gives the size of the cascade to which the repective column bleongs.
#' The method returns NULL if the object subcascades is empty.
#' 
#' 
#' @seealso \code{\link{groupwise}}, \code{\link{as.subcascades}}
#' 
#' @examples 
#' library(TunePareto)
#' library(igraph)
#' data(esl)
#' data = esl$data
#' labels = esl$labels
#' predMap = predictionMap(data, labels, 
#'                        classifier = tunePareto.svm(), kernel='linear')
#' 
#' # generate a dataframe
#' subcascades = subcascades(predMap,thresh=0.65,size=4)
#' edges = as.edgedataframe(subcascades)
#' g = graph_from_data_frame(edges[,c(1,2)], directed = TRUE)
#' E(g)$weight = edges[,3]
#' plot(g,edge.color=edges[,3],edge.arrow.size=0.5,
#'      edge.curved =seq(-0.5, 1, length = ecount(g)))
as.edgedataframe <- function(subcascades){
  
  #################################################
  ##
  ## Check parameter 'subcascades'
  
  if(is.null(subcascades))
    return(NULL)
  
  if(!inherits(subcascades, 'Subcascades'))
    stop(errorStrings('subcascades'))
  
  #################################################
  
  cascades = lapply(subcascades, rownames)

  el = c()
  tmp=1
  for(name.group in 1:length(cascades)){
    name.group.cur = cascades[[name.group]]
    cascs = sapply(name.group.cur,function(x){strsplit(x,'>')})
    for(casc in 1:length(cascs)){
      casc.cur = cascs[[casc]]
      for(pair in 1:(length(casc.cur)-1)){
        el = rbind(el,c(as.numeric(casc.cur[pair]),as.numeric(casc.cur[pair+1]),tmp,names(cascades)[name.group]))
      }
      tmp = tmp+1
    }
  }
  el = as.data.frame(el)
  colnames(el) = c('V1','V2','CASC_ID','SIZE')
  return(el)
}