#' Heatmap of a Subcascades Object 
#' 
#' Plots a heatmap, that shows the performance of cascades of a Subcascades object.
#' 
#' @param x
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function. 
#' @param class.sort
#' The classes can be sorted either according to the first cascade in the subcacades object (''), 
#' or based on the class frequency ('max').
#' @param row.sort
#' The cascade can be sorted either based on the maximal-minimal class-wise sensitivity ('sens') or 
#' according the class frequency.
#' @param thresh
#' A numeric value between 0 and 1 that is
#' the minimal sensitivity of the Subcascades object
#' used as lower bound for the color scaling. 
#' @param main
#' See \code{\link{plot}}.
#' @param xlab
#' A title for the x axis (see \code{\link{plot}}).
#' @param ylab
#' A title for the y axis (see \code{\link{plot}}).
#' @param digits
#' Integer indicating the number of decimal places to be used (see \code{\link{round}}).
#' @param casc.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity. 
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param color.key
#' Specifies whether a color key is drawn (TRUE) or not (FALSE).
#' @param na.color 
#' Color, which is used for indicating the empty elements (if the given class is not part of the cascade).
#' @param ignore
#' A numeric value between 0 and 1. 
#' All confusion and purity values below this number are not written as string into the corresponding element. 
#' @param las
#' See \code{\link{par}}.
#' @param cex
#' See \code{\link{par}}.
#' @param cex.lab
#' See \code{\link{par}}.
#' @param srt
#' Angle used to rotate the strings of the x-axis and y-axis labels (see \code{\link{par}}).
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @return 
#' No return value, called to generate the heatmap plot of the subcascades Object.
#' 
#' @details 
#' This function plots a heatmap with the cascades of the Subcascades object in the rows 
#' and all classes present in any of the cascades in the columns. 
#' The colors indicate whether a given class is present in the corresponding cascade and with which sensitivity.
#' 
#' @seealso \code{\link{subcascades}}, \code{\link{plot.Conf}}, \code{\link{plot.ConfusionTable}}
#' 
#' @examples 
#' library(TunePareto)
#' data(esl)
#' data <- esl$data
#' labels <- esl$labels
#' foldList <- generateCVRuns(labels  = labels,
#'                           ntimes      = 2,
#'                           nfold       = 2,
#'                           leaveOneOut = FALSE,
#'                           stratified  = TRUE)
#' genMap <- gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subcascades <- subcascades(genMap,thresh=0.7,size=c(3,4))
#' 
#' plot(subcascades,thresh=0.7,row.sort='max')
#' 
#' 


plot.Subcascades <- function( x       = NULL,
                                class.sort = '',
                                row.sort = 'sens',
                                thresh = 0,
                                 main    = 'subcascades',
                                 xlab    = 'classes',
                                 ylab    = 'cascades',
                                 digits  = 3,
                              ignore       = 0,
                                 casc.colors  = c('#f5f5f5','#01665e'),
                                 na.color     = c('#f5f5f5'),
                                color.key = TRUE,
                                 las=1,
                                 cex = 1,
                                 cex.lab = 1,
                                 srt = 30,
                                 ...)
{ subcascades <- x
  #################################################
  ##
  ## Check parameter 'resultMatrix'
  
  if(is.null(subcascades))
    stop(errorStrings('subcascadesMissing'))
  
  if(!inherits(subcascades, 'Subcascades'))
    stop(errorStrings('subcascades'))
  
  #################################################
  oldpar <- par(no.readonly = TRUE)    
  on.exit(par(oldpar))
  
  classes <- unique(unname(unlist(lapply(subcascades,function(x){strsplit(rownames(x),'>')}))))
  resultMatrix <- matrix(0,sum(unlist(lapply(subcascades,nrow))),length(classes))
  colnames(resultMatrix) <- as.character(classes)
  tmp <- 1
  tmp1 <- c()
  for (i in seq_along(subcascades)){
    size.cur <- subcascades[[i]]
    for (j in seq_len(nrow(size.cur))){
      resultMatrix[tmp,(strsplit(rownames(size.cur)[j],'>')[[1]])] <- size.cur[j,]
      tmp <- tmp+1
    }
  }
  cascs <- lapply(subcascades,rownames)
  rownames(resultMatrix) <- unlist(cascs)
  
  if(class.sort == 'max'){
    res.sort <- resultMatrix
    res.sort[res.sort>0] <- 1
    resultMatrix <- resultMatrix[,order(colSums(res.sort),decreasing=TRUE)]
  }

  if (row.sort == 'sens'){
    sens <- apply(resultMatrix,1,min)
    resultMatrix <- resultMatrix[order(sens),]
  }else{
    res.sort <- resultMatrix
    res.sort[res.sort>0] <- 1
    for (i in rev(seq_len(ncol(resultMatrix)))){
      tmp <- order(res.sort[,i],decreasing=TRUE)
      res.sort <- res.sort[tmp,]
      resultMatrix <- resultMatrix[tmp,]
    }
  }

  #################################################
  ##
  ## Preparation
  
  
  cascRamp  <- colorRampPalette(casc.colors)(101)
  numClasses <- nrow(resultMatrix)
  xbox <- c(-0.5,-0.5,0.5,0.5)
  ybox <- c(-0.5,0.5,0.5,-0.5)
  ylim <- c(0.5, nrow(resultMatrix)+0.5)
  
  colnms <- colnames(resultMatrix)
  rownms <- rownames(resultMatrix)
  
  margin.extend <- max(c(strwidth(unname(rownms), units='inches'),strwidth(unname(colnms), units='inches')))
  mai <- c(margin.extend*1.1, margin.extend*1.1,par()$mai[c(3,4)])
  par(mai = mai)
  
  extension <- 0
  a1.label <- c()
  a1.pos <- c()
  a3.label <- c()
  a3.pos <- c()
  if(color.key)
  {
      extension <- 2
      a1.label <- 'casc'
      a1.pos  <- ncol(resultMatrix)+2
      
      a3.label <- 'color key'
      a3.pos <- ncol(resultMatrix)+2
  }
  
  ###################################################
  ###
  ### plot background

  plot(   0,0,
          type = 'n',
          xlim = c(0.5,ncol(resultMatrix)+0.5+extension),
          ylim = ylim,
          main = main,
          xlab = xlab,
          ylab = '',
          xaxt = 'n',
          yaxt = 'n',
          yaxs = 'i',
          xaxs = 'i',
          bty = 'n',
          cex.lab = cex.lab,
          ...)
  
  ###################################################
  ###
  ### plot axis and labels
  
  
  par(oma=c(0,cex.lab,0,0))
  axis(1, seq_len(ncol(resultMatrix)),  labels = FALSE, las=las, ...)
  text(x = seq_len(ncol(resultMatrix)), par('usr')[3] - 0.2, labels = colnms, srt = srt, pos = 1, xpd = TRUE)
  mtext(side = 2, text = ylab,outer=TRUE,line=0)
  axis(2, seq_len(nrow(resultMatrix)),  labels = FALSE, las=las, ...)
  text(y = seq_len(nrow(resultMatrix)), par('usr')[3], labels = rownms, srt = srt, pos = 2, xpd = TRUE)

  if(color.key)
  {
    axis(1, a1.pos[1], a1.label[1], las=las, ...)
    axis(3, a3.pos, a3.label,line = -1,  las=1, tick = FALSE, ...)
    text(x=a1.pos[1]-1, y=mean(ylim),labels = 'sensitivity', srt = 90, cex = cex.lab)
  }
  
  ###################################################
  ###
  ### plot color key
  
  if(color.key)
  {
    axis(4, ylim, c(thresh,1), las=1, ...)
    
    numCol <- 101
    color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numCol)
    
    polygon( x   = xbox+a1.pos[1],
             y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
             col = 'white',
             lwd = 2)
    
    sapply(2:length(color.bounds), function(i){
      polygon( x      = xbox+a1.pos[1],
               y      = color.bounds[c(i-1, i, i, i-1)],
               col    = cascRamp[(i/numCol)*100],
               border = NA)
    })

  }
  
  ###################################################
  ###
  ### plot extended confusion table
  
  coordinates <- expand.grid(y=seq_len(nrow(resultMatrix)),x=seq_len(ncol(resultMatrix)))
  
  colors <- apply(coordinates,1,function(x){
    i <- x[1]
    j <- x[2]
    
    val <- round(resultMatrix[i,j], digits = digits)
    if(val == 0){
      return(na.color)
    }else{
      val <- (val - thresh) / (1 - thresh)
      return(cascRamp[val*100+1])
    }
  })
  

  texts <- apply(coordinates,1,function(x){
    i <- x[1]
    j <- x[2]
    
    return(round(resultMatrix[i,j], digits = digits))
  })
  texts[texts<=ignore] <- NA
  
  rect(   coordinates[,'x']-0.5,
          coordinates[,'y']-0.5,
          coordinates[,'x']+0.5,
          coordinates[,'y']+0.5,
          col = colors,
          border = 'black')
  
  text(coordinates[,'x'], coordinates[,'y'] ,texts,cex = cex)
  ###################################################


  
}
