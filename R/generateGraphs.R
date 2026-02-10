#' Generate a dot string graph for the obtained cascades/subcascades
#'
#' @importFrom stringr str_sort str_replace_all
#' @importFrom plotrix color.id
#' @importFrom utils tail
#'
#' @param subcascade Subcascades object as returned by \code{\link{subcascades}}-function.
#' @param categoricalLabels Optional factor or character vector of class labels.
#' @param numericLabels Optional numeric vector of class labels.
#' @param useOldLabels Logical; whether to map numeric labels to categorical labels.
#' @param simplify_iter Integer; number of simplification iterations.
#' @param graphName Optional string for graph name
#'
#' @export
###################################################################
###
generateGraphs <- function(subcascade=NULL, useOldLabels=F, numericLabels=NULL, simplify_iter=0, categoricalLabels=NULL, graphName=NULL){
  pths <- get_paths(subcascade)

  if(length(categoricalLabels)!=0 && useOldLabels==T){
    pths <- map_old_labs(pths, numericLabels, categoricalLabels)
  }

  pths <- paste0("START>",pths, ">END")

  ### check
  for(i in 1:simplify_iter){
    pths <- merge_alt_nodes(pths)
    pths <- merge_sequence_nodes(pths)
  }
  pths <- matrix(sort(pths))

  edges <- pths_to_edges(pths)
  nodes <- pths_to_nodes(pths)

  pths <- simplify_paths(pths)

  grDot <- gen_dot_string_pths(pths, edges,nodes, name=graphName)
  #dot(grDot)
}
