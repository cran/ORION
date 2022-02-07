#' Base Classifier Performance Heatmap
#' 
#' Plots a heatmap that shows base classifier performance.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Subcascades
#' @param x
#' A Conf object as it is returned by \code{\link{conf}}-function. 
#' @param classNames
#' Vector of the original class names. If not given the class number is used instead.
#' @param first.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the first class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param second.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the second class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param other.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the other class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function plots a heatmap of the base classifier performance.
#' 
#' @seealso \code{\link{conf}}, \code{\link{plot.Subcascades}}, \code{\link{plot.ConfusionTable}}
#' 
#' @return 
#' No return value, called to generate the heatmap plot.
plotConf <- function( x            = NULL,
                        classNames   = NULL,
                        main         = 'summary of base classifiers',
                        xlab         = 'classes',
                        ylab         = 'base classifiers',
                        digits       = 3,
                        ignore       = 0,
                        first.colors  = c('#f5f5f5','#b2182b'),
                        second.colors = c('#f5f5f5','#2166ac'),
                        other.colors = c('#f5f5f5','#1b7837'),
                        las=1,
                        srt = 30,
                        color.key = FALSE,
                        cex = 1,
                        cex.lab = 1,
                        ...)
{
  #################################################
  ##
  ## Check parameter 'x'
  
  if(is.null(x))
    stop(errorStrings('confMissing'))
  
  if(!inherits(x, 'Conf'))
    stop(errorStrings('conf'))
  
  ###########################################################
  ###
  ### Extract sensitivities
  
  numClasses <- sqrt(length(x$fC))
  
  classes <- 0:(numClasses-1)
  
  combs <- expand.grid(classes,classes)
  combs <- as.matrix(combs[combs[,1] != combs[,2],, drop = FALSE])[,2:1]
  
  firstClass <- t(sapply(1:nrow(combs),function(i){
    c(combs[i,1],i, x$fC[paste('[',combs[i,1],'vs',combs[i,2],']',sep = ''),])
  }))
  
  secondClass <- t(sapply(1:nrow(combs),function(i){
    c(combs[i,2],i, x$sC[paste('[',combs[i,1],'vs',combs[i,2],']',sep = ''),paste('TN',combs[i,2],sep = '')])
  }))
  
  otherClasses <- do.call("rbind",lapply(1:nrow(combs), function(i){
    y <- combs[i,]
    
    sens <- t(sapply(classes[-(y+1)], function(z){
      c(z, x$sC[paste('[',y[1],'vs',y[2],']',sep = ''),paste('TN',z,sep = '')])
    }))
    
    return(cbind(x=sens[,1], y=i, sens=sens[,2]))
  }))
  
  indices <- list(  firstClass = firstClass,
                    secondClass=secondClass,
                    otherClasses = otherClasses)
  
  indices <- lapply(indices, function(y){
    colnames(y) <- c("x","y","sens")
    y[,1] <- y[,1]+1
    y[,2] <- nrow(combs)-y[,2]+1
    return(y)
  })
  
  
  #################################################
  ##
  ## Preparation
  
  numColors <- 101
  ramps <- list( firstRamp = colorRampPalette(first.colors)(numColors),
                 secondRamp= colorRampPalette(second.colors)(numColors),
                 otherRamp = colorRampPalette(other.colors)(numColors))
  
  nrow = nrow(combs)
  
  xbox <- c(-1,-1,1,1) * 0.25
  ylim <- c(0.5, nrow+0.5)
  
  rownms <- rev(apply(combs,1,function(y){
    paste('[',y[1],'vs',y[2],']',sep = '')
  }))
  
  ###################################################
  ###
  ### main plot
  
  plot.default(   0,0,
          type = 'n',
          xlim = c(0.5,numClasses+0.5+3.5*color.key),
          ylim = ylim,
          main = main,
          xlab = "",
          ylab = ylab,
          xaxt = 'n',
          yaxt = 'n',
          yaxs = 'i',
          xaxs = 'i',
          bty = 'n',
          cex.lab = cex.lab,
          las = 0,
          ...)
  
  axis(1, classes+1, classes, las=las, ...)
  text(x = ceiling(numClasses/2), par('usr')[3], labels = xlab, pos = 1, offset=3.4, xpd = TRUE)
  
  #mtext(side = 2, text = ylab,outer=TRUE,line=0)
  #axis(2, setdiff(seq_len(nrow), nrow-1),  labels = FALSE, las=las, ...)
  #text(y = seq_len(nrow), par('usr')[3], labels = rownms, srt = srt, pos = 2, xpd = TRUE)
  axis(2, seq_len(nrow), labels = rownms, las=las, ...)
  
  
  polygon( x   = xbox,
           y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
           col = 'white',
           lwd = 2)
  
  rect(   0.5,
          0.5,
          numClasses+0.5,
          nrow+0.5,
          col = "white",
          border = 'black')
  
  for(i in 3:1){
    rect(   indices[[i]][,"x"]-0.5,
            indices[[i]][,"y"]-0.5,
            indices[[i]][,"x"]+0.5,
            indices[[i]][,"y"]+0.5,
            col = ramps[[i]][(indices[[i]][,"sens"]*100)+1],
            border = 'black',
            lwd = 1)
  }
  
  lapply(indices, function(ind){
    ind[,"sens"] <- round(ind[,"sens"], digits=digits)
    ind[ind[,"sens"]<= ignore ,"sens"] <- NA
    text(ind[,"x"], ind[,"y"] ,ind[,"sens"],cex = cex)
  })
  
  ###################################################
  ###
  ### color.key
  
  if(color.key)
  {
    color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
    
    xcoord   <- rep(numClasses+0.5,numColors)
    ycoord.l <- color.bounds[1:numColors]
    ycoord.u <- color.bounds[2:(numColors+1)]
    
    for(i in 1:3){
      polygon( x   = xbox+i+numClasses+0.5,
               y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
               col = 'white',
               lwd = 2)
      
      rect(   xcoord+i-0.25,
              ycoord.l,
              xcoord+i+0.25,
              ycoord.u,
              col = ramps[[i]],
              border = NA)
    }
    
    axis(1, 1:3+numClasses+0.5,  labels = FALSE, las=las, ...)
    text(x = 1:3+numClasses+0.5, par('usr')[3], labels = c("first", "second", "others"), srt = srt, pos = 1, offset=1, xpd = TRUE)
    
    text(x = 1:3+numClasses, y = nrow/2, labels = c("sensitivity for first class", "sensitivity for second class", "predicting other classes as second class"), srt = 90, xpd = TRUE)
    
    axis(4,range(color.bounds),c(0,1),las=2, ...)
    text(x = 2+numClasses+0.5, par('usr')[3], labels = "legend", pos = 1, offset=3.4, xpd = TRUE)
  }
}

#' Base Classifier Performance Heatmap
#' 
#' Plots a heatmap that shows base classifier performance.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Subcascades
#' @param x
#' A Conf object as it is returned by \code{\link{conf}}-function. 
#' @param classNames
#' Vector of the original class names. If not given the class number is used instead.
#' @param symmetric
#' Logical indicating whether a symmetric base classifier (TRUE) is used.
#' @param first.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the first class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param second.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the second class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function plots a heatmap of the base classifier performance.
#' 
#' @return 
#' No return value, called to generate the heatmap plot.
plotBaseClassifier <- function (   x = NULL,
                                    classNames = NULL,
                                    symmetric = FALSE,
                                    main = "summary of base classifiers",
                                    xlab = "second class",
                                    ylab = "first class",
                                    digits = 3,
                                    ignore = 0,
                                    first.colors = c("#f5f5f5", "#8c510a"),
                                    second.colors = c("#f5f5f5","#01665e"),
                                    na.color = c("yellow"),
                                    las = 1,
                                    color.key = TRUE,
                                    cex = 1,
                                    cex.lab = 1,
                                    ...)
{
  #################################################
  ##
  ## Check parameter 'x'
  
  if (is.null(x))
    stop(errorStrings("confMissing"))
  
  if (!inherits(x, "Conf"))
    stop(errorStrings("conf"))
  
  ###########################################################
  ###
  ### Extract sensitivities
  
  numClasses <- sqrt(length(x$fC))
  meanClasses  <- (numClasses+(1-numClasses%%2))/2
  
  classes <- 0:(numClasses-1)
  
  combs <- expand.grid(classes,classes)
  combs <- as.matrix(combs[combs[,1] != combs[,2],, drop = FALSE])[,2:1]
  
  firstClass <- t(apply(combs,1,function(y){
    c(y[1],y[2], x$fC[paste('[',y[1],'vs',y[2],']',sep = ''),])
  }))
  
  secondClass <- t(apply(combs,1,function(y){
    c(y[1],y[2], x$sC[paste('[',y[1],'vs',y[2],']',sep = ''),paste('TN',y[2],sep = '')])
  }))
  
  indices <- list(  firstClass = firstClass,
                    secondClass= secondClass)
  
  indices <- lapply(indices, function(y){
    colnames(y) <- c("x","y","sens")
    y[,1] <- y[,1]+1
    y[,2] <- numClasses-y[,2]
    return(y)
  })
  
  #################################################
  ##
  ## Preparation
  
  numColors <- 101
  ramps <- list( firstRamp = colorRampPalette(first.colors)(numColors),
                 secondRamp= colorRampPalette(second.colors)(numColors))
  nrow = nrow(combs)
  
  xbox <- c(-1,-1,1,1) * 0.25
  ylim <- c(0.5, numClasses+0.5)
  
  colnms <- rev(classes)
  rownms<- classes
  
  #################################################
  ##
  ## Save global parameters
  
  xbox <- c(-1,-1,1,1) * 0.25
  ylim <- c(0.5, numClasses + 0.5)
  
  if (!is.null(classNames)) {
    if (max(c(colnms + 1, rownms + 1)) > length(classNames))
      stop(errorStrings("classNames"))
    colnms <- classNames[colnms + 1]
    rownms <- classNames[rownms + 1]
  }
  
  ###################################################
  ###
  ### main plot
  
  plot.default(   0, 0,
          type = "n",
          xlim = c(0.5, (numClasses + 0.5)*2^(1-symmetric) + 1.5*color.key*(1+(1-symmetric)) ),
          ylim = ylim,
          main = paste(main,"\n"),
          xlab = "",
          ylab = ylab,
          xaxt = "n",
          yaxt = "n",
          yaxs = "i",
          xaxs = "i",
          bty = "n",
          cex.lab = cex.lab,
          ...)
  
  axis(2, classes+1, colnms, las = las, ...)
  
  ###################################################
  ###
  ### main plot
  
  if(symmetric){
    captions <- "symmetric base classifier"
  }else{
    captions <- c("sensitivity for first class","sensitivity for second class")
  }
  
  coords.y <- rev(classes)
  
  for (i in 1:(1+(1-symmetric)))
  {
    off <- (i-1)*(numClasses+1)
    
    rect(   0.5 + off,
            0.5,
            numClasses+0.5 + off,
            numClasses+0.5,
            col = "white",
            border = 'black')
    
    rect(   indices[[i]][,"x"]-0.5+off ,
            indices[[i]][,"y"]-0.5,
            indices[[i]][,"x"]+0.5+off,
            indices[[i]][,"y"]+0.5,
            col = ramps[[i]][(indices[[i]][,"sens"]*100)+1],
            border = 'black',
            lwd = 1)
    
    rect(   classes+1-0.5+off ,
            coords.y+1-0.5,
            classes+1+0.5+off,
            coords.y+1+0.5,
            col = na.color,
            border = 'black',
            lwd = 1)
    
    indices[[i]][,"sens"] <- round(indices[[i]][,"sens"], digits=digits)
    indices[[i]][indices[[i]][,"sens"]<= ignore ,"sens"] <- NA
    text(indices[[i]][,"x"]+off,  indices[[i]][,"y"] ,indices[[i]][,"sens"],cex = cex)
    
    axis(1, classes+1+off, rownms, las = las, ...)
    
    text(x = meanClasses+off+0.5, par('usr')[3], labels = xlab, pos = 1, offset=3.4, xpd = TRUE)
    
    axis(3, meanClasses+off+0.5, captions[i] , las = 1, tick = FALSE,...)
  }
  
  ###################################################
  ###
  ### color.key
  
  if(color.key)
  {
    color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
    
    xcoord   <- rep(numClasses+0.5,numColors)
    ycoord.l <- color.bounds[1:numColors]
    ycoord.u <- color.bounds[2:(numColors+1)]
    
    off <- (1-symmetric)*(numClasses+1)
    
    labels      = c("sensitivity for first class", "sensitivity for second class")
    description = c("first", "second")
    
    for(i in 1:(1+(1-symmetric))){
      polygon( x   = xbox+i+numClasses+0.5+off,
               y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
               col = 'white',
               lwd = 2)
      
      rect(   xcoord+i-0.25+off,
              ycoord.l,
              xcoord+i+0.25+off,
              ycoord.u,
              col = ramps[[i]],
              border = NA)
      
      srt = 30
      text(x = i+off+numClasses+0.5, par('usr')[3], labels = description[i], srt = srt, pos = 1, offset=1, xpd = TRUE)
      
      text(x = i+off+numClasses, y = mean(ylim) ,labels = labels[i], srt = 90, xpd = TRUE)
    }
    axis(1, 1:(1+(1-symmetric))+off+numClasses+0.5,  labels = FALSE, las=las, ...)
    axis(4, ylim, c(0,1), las=1, ...)
  }
  
}

#' Base Classifier Performance Heatmap
#' 
#' Plots a heatmap that shows base classifier performance.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Subcascades
#' @param x
#' A Conf object as it is returned by \code{\link{conf}}-function. 
#' @param classNames
#' Vector of the original class names. If not given the class number is used instead.
#' @param onlySens
#' A logical indicating if the plot should should be restricted to the class-wise sensitivities of the base classifiers.
#' @param symmetric
#' Logical indicating whether a symmetric base classifier (TRUE) is used. Only used when onlySens is TRUE.
#' @param first.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the first class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param second.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the second class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param other.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the other class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function plots a heatmap of the base classifier performance.
#' 
#' @seealso \code{\link{conf}}, \code{\link{plot.Subcascades}}, \code{\link{plot.ConfusionTable}}
#' 
#' @return 
#' No return value, called to generate the heatmap plot.
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' conf <- conf(predMap)
#' 
#' plot(conf, onlySens=TRUE, symmetric=TRUE)
plot.Conf <- function( x            = NULL,
                      classNames   = NULL,
                      onlySens = FALSE,
                      symmetric    = FALSE,
                      main         = 'summary of base classifiers',
                      xlab         = 'classes',
                      ylab         = 'base classifiers',
                      digits       = 3,
                      ignore       = 0,
                      first.colors  = c('#f5f5f5','#b2182b'),
                      second.colors = c('#f5f5f5','#2166ac'),
                      other.colors = c('#f5f5f5','#1b7837'),
                      na.color = c("yellow"),
                      las=1,
                      srt = 30,
                      color.key = FALSE,
                      cex = 1,
                      cex.lab = 1,
                      ...) {
  if(onlySens) {
    plotBaseClassifier(x            = x,
                       classNames   = classNames,
                       symmetric    = symmetric,
                       main         = main,
                       xlab         = xlab,
                       ylab         = ylab,
                       digits       = digits,
                       ignore       = ignore,
                       first.colors  = first.colors,
                       second.colors = second.colors,
                       na.color = na.color,
                       las = las,
                       srt = srt,
                       color.key = color.key,
                       cex = cex,
                       cex.lab = cex.lab,
                       ...)
  }else {
    plotConf(             x            = x,
                          classNames   = classNames,
                          main         = main,
                          xlab         = xlab,
                          ylab         = ylab,
                          digits       = digits,
                          ignore       = ignore,
                          first.colors  = first.colors,
                          second.colors = second.colors,
                          other.colors = other.colors,
                          las = las,
                          srt = srt,
                          color.key = color.key,
                          cex = cex,
                          cex.lab = cex.lab,
                          ...)
  }
}

#' Extended Confusion Table Plot
#' 
#' Plots an extended confusion table.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Conf
#' @param x
#' A ConfusionTable object as it is returned by \code{\link{confusionTable}}-function. 
#' @param cascLab
#' Character string used as header for the cascade part of the extended confusion table.
#' @param otherLab
#' Character string used as header for the other class part of the extended confusion table.
#' @param colSep
#' Color, which is used for the vertical line separating the cascade classes and the other classes.
#' @param casc.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the first class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' @param other.colors
#' A 2-element vector of the color for the minimal and maximal class-wise sensitivity of the second class.
#' The color palette is calcuated by an interpolation between the 2 given colors.
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @return 
#' No return value, called to generate the confusion table plot.
#' 
#' @seealso \code{\link{confusionTable}}, \code{\link{plot.Subcascades}}, \code{\link{plot.Conf}}
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' conf.table <- confusionTable(predMap,cascade='0>1>3>4',other.classes = 'all')
#' 
#' plot(conf.table)
plot.ConfusionTable <- function( x       = NULL,
                                  classNames = NULL,
                                  main    = 'extended confusion table',
                                  xlab    = 'original class labels',
                                  ylab    = 'predicted class labels',
                                  cascLab = 'inner classes',
                                  otherLab= 'outer classes',
                                  digits  = 3,
                                  ignore  = 0,
                                  casc.colors  = c('#f5f5f5','#8c510a'),
                                  other.colors = c('#f5f5f5','#01665e'),
                                  colSep = '#b2182b',
                                  las=1,
                                  color.key = TRUE,
                                  cex = 1,
                                  cex.lab = 1,
                                  ...)
{  
  #################################################
  ##
  ## Check parameter 'confusionTable'
  
  if(is.null(x))
    stop(errorStrings('confusionTableMissing'))
  
  if(!inherits(x, 'ConfusionTable'))
    stop(errorStrings('confusionTable'))
  
  #################################################
  ##
  ## Preparation
  
  x <- round(x[nrow(x):1,], digits=digits)
  
  numAll  <- ncol(x)
  numOrig <- nrow(x)
  numOther<- numAll-numOrig
  
  #meanAll  <- (numAll+(1-numAll%%2))/2
  meanAll <- (numAll+1)/2
  #meanOrig <- (numOrig+(1-numOrig%%2))/2
  meanOrig <- (numOrig+1)/2
  
  tmp      <- (numAll-numOrig+1)
  meanOther <- tmp/2 + numOrig
  #meanOther<- ((tmp+(1-tmp%%2))/2) + numOrig
  
  exist.foreign.classes <- numOther>0
  
  numColors <- 101
  colorRamps<- list(cascRamp = colorRampPalette(casc.colors)(numColors),
                    otherRamp= colorRampPalette(other.colors)(numColors))
  
  xbox <- c(-1,-1,1,1) * 0.25 #c(-0.5,-0.5,0.5,0.5)
  ylim <- c(0.5, nrow(x)+0.5)
  
  colnms <- as.numeric(sapply(colnames(x), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
  rownms <- as.numeric(sapply(rownames(x), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
  
  if(!is.null(classNames))
  {   
    if(max(c(colnms+1,rownms+1))>length(classNames))
      stop(errorStrings('classNames'))
    
    colnms <- classNames[colnms+1]
    rownms <- classNames[rownms+1]
  }
  
  if(exist.foreign.classes){
    extension <- 5
  }else{
    extension <- 3
  }
  
  ###################################################
  ###
  ### plot background
  
  plot(   0,0,
          type = 'n',
          xlim = c(0.5,ncol(x)+0.5+extension),
          ylim = ylim,
          main = paste(main, "\n"),
          xlab = "",
          ylab = ylab,
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
  
  axis(1, seq_len(ncol(x)), colnms, las=las, ...)
  axis(2, seq_len(nrow(x)), rownms, las=las, ...)
  axis(3, meanOrig, cascLab, las=1, tick = FALSE, ... )
  
  text(x = meanAll, par('usr')[3], labels = xlab, pos = 1, offset=3.4, xpd = TRUE, cex = cex.lab)
  
  if(exist.foreign.classes)
    axis(3, meanOther, otherLab, las=1, tick = FALSE, ... )
  
  ###################################################
  ###
  ### main plot
  
  coordinates <- expand.grid(y=seq_len(nrow(x)),x=seq_len(ncol(x)))
  
  colors <- apply(coordinates,1,function(y){
    if(y[2] <= numOrig)
    {
      colorRamps$cascRamp[x[y[1],y[2]]*100+1]
    }else{
      colorRamps$otherRamp[x[y[1],y[2]]*100+1]
    }
  })
  
  texts <- apply(coordinates,1,function(y){x[y[1],y[2]]})
  texts[texts<=ignore] <- NA
  
  rect(   coordinates[,'x']-0.5,
          coordinates[,'y']-0.5,
          coordinates[,'x']+0.5,
          coordinates[,'y']+0.5,
          col = colors,
          border = 'black')
  
  text(coordinates[,'x'], coordinates[,'y'], texts, cex = cex)
  
  if(exist.foreign.classes)
    abline(v = nrow(x)+0.5, col = colSep, lwd=4)
  
  ###################################################
  ###
  ### plot color key
  
  if(color.key)
  {
    color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
    
    xcoord   <- rep(numAll,numColors)+1
    ycoord.l <- color.bounds[1:numColors]
    ycoord.u <- color.bounds[2:(numColors+1)]
    
    description <- c("inner", "outer")
    labels      <- c("confusion", "external rates")
    index       <- 1:(1+exist.foreign.classes)
    
    for(i in index)
    {
      polygon( x   = xbox+2*i+numAll+1,
               y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
               col = 'white',
               lwd = 2)
      
      rect(   xcoord+2*i-0.25,
              ycoord.l,
              xcoord+2*i+0.25,
              ycoord.u,
              col = colorRamps[[i]],
              border = NA)
      
      text(x=numAll+(2*i), y=mean(ylim),labels = labels[i], srt = 90, cex = cex.lab)
      
      text(x = 2*i+numAll+1, par('usr')[3], labels = description[i], srt = 30, pos = 1, offset=1, xpd = TRUE, cex = cex.lab)
      
    }
    axis(1, 2*index+numAll+1,  labels = FALSE, las=las, ...)
    text(x = mean(2*index+numAll+1), par('usr')[3], labels = "legend", pos = 1, offset=3.4, xpd = TRUE, cex = cex.lab)
    axis(4, ylim, c(0,1), las=1, ...)    
  }
}

#' Heatmap of a Subcascades Object 
#' 
#' Plots a heatmap, that shows the performance of cascades of a Subcascades object.
#' 
#' @param x
#' A Subcascades object as it is returned by \code{\link{subcascades}}-function. 
#' @param classNames
#' Character vector including the names of the classes. 
#' @param class.sort
#' The classes can be sorted either by:
#' "" = unsorted, "max": sorted according to maximal occurrence of a class or 
#' "sens": sorted according to highest class-wise sensitivity
#' @param row.sort
#' The cascade can be sorted by:
#' "" = unsorted, "max": sorted according to maximal occurrence of maximal length or 
#' "sens": sorted according to highest class-wise sensitivity
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc <- subcascades(predMap,thresh=0.7,size=c(3,4))
#' 
#' plot(subc,row.sort='max')
#' 
plot.Subcascades <- function( x       = NULL,
                              classNames = NULL,
                               class.sort = '',
                               row.sort = '',
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
{ 
  #################################################
  ##
  ## Check parameter 'resultMatrix'
  
  if(is.null(x))
    stop(errorStrings('subcascadesMissing'))
  
  if(!inherits(x, 'Subcascades'))
    stop(errorStrings('subcascades'))
  
  #################################################
  ##
  ## Generate resultMatrix
  
  cascades <- unname(unlist(lapply(x,rownames)))
  classes <- unique(unname(unlist(strsplit(cascades,'>'))))
  
  resultMatrix <- matrix(0,length(cascades),length(classes))
  colnames(resultMatrix) <- as.character(classes)
  rownames(resultMatrix) <- cascades
  
  for (i in seq_along(cascades))
  {
    cls <- strsplit(cascades[i],'>')[[1]]
    resultMatrix[i,cls] <- x[[paste("size.",length(cls),sep="")]][cascades[i],]
  }
  
  numClasses   <- ncol(resultMatrix)
  numCascades  <- nrow(resultMatrix)
  meanClasses  <- (numClasses+(1-numClasses%%2))/2
  
  #################################################
  ##
  ## Sorting
  
  if(class.sort == 'max')
    resultMatrix <- resultMatrix[,order(colSums(resultMatrix>0),decreasing=TRUE)]
  
  if (row.sort == 'sens'){
    sens <- apply(resultMatrix,2, function(y){min(y[y!=0])})
    resultMatrix <- resultMatrix[,order(sens)]
  }
  
  if (row.sort == 'sens'){
    sens <- apply(resultMatrix,1, function(y){min(y[y!=0])})
    resultMatrix <- resultMatrix[order(sens),]
  }
  
  if (row.sort == 'max'){
    resultMatrix <- resultMatrix[order(rowSums(resultMatrix>0)),]
  }
  
  #################################################
  ##
  ## Preparation plot
  
  numColors <- 101
  cascRamp  <- colorRampPalette(casc.colors)(numColors)
  
  xbox <- c(-1,-1,1,1) * 0.25
  ylim <- c(0.5, numCascades+0.5)
  
  colnms <- colnames(resultMatrix)
  rownms <- rownames(resultMatrix)
  
  if (!is.null(classNames)) {
    if (max(c(colnms + 1, rownms + 1)) > length(classNames))
      stop(errorStrings("classNames"))
    colnms <- classNames[colnms + 1]
    rownms <- classNames[rownms + 1]
  }
  
  
  ###################################################
  ###
  ### plot background
  
  plot(   0,0,
          type = 'n',
          xlim = c(0.5,numClasses+0.5+1.5*color.key),
          ylim = ylim,
          main = main,
          xlab = "",
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
  axis(1, seq_len(numClasses),  labels = colnms, las=las, ...)
  text(x = meanClasses, par('usr')[3], labels = xlab, pos = 1, offset=3.4, xpd = TRUE, cex = cex.lab)
  
  
  axis(2, seq_len(numCascades), rownms, las = las, ...)
  
  ###################################################
  ###
  ### plot heatmap
  
  coordinates <- expand.grid(y=seq_len(numCascades),x=seq_len(numClasses))
  
  colors <- apply(coordinates,1,function(x){
    val <- resultMatrix[x[1],x[2]]
    if(val == 0){
      return(na.color)
    }else{
      return(cascRamp[val*100+1])
    }
  })
  
  resultMatrix <- round(resultMatrix,digits=digits)
  
  texts <- apply(coordinates,1,function(x){
    return(round(resultMatrix[x[1],x[2]], digits = digits))
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
  ###
  ### plot color key
  
  if(color.key)
  {
    color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
    
    xcoord   <- rep(numClasses+0.5,numColors)
    ycoord.l <- color.bounds[1:numColors]
    ycoord.u <- color.bounds[2:(numColors+1)]
    
    polygon( x   = xbox+numClasses+1+0.5,
             y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
             col = 'white',
             lwd = 2)
    
    rect(   xcoord+1-0.25,
            ycoord.l,
            xcoord+1+0.25,
            ycoord.u,
            col = cascRamp,
            border = NA)
    
    axis(4, ylim, c(0,1), las=1, ...)
    
    axis(1, numClasses+1.5, labels = FALSE, las=las, ...)
    
    text(x=numClasses+1, y=mean(ylim),labels = 'sensitivity', srt = 90, cex = cex.lab)
    text(x = numClasses+1.5, par('usr')[3], labels = "sensitivity", srt = srt, pos = 1, offset=1, xpd = TRUE)
    text(x = numClasses+1.5, par('usr')[3], labels = "legend", pos = 1, offset=3.4, xpd = TRUE)
  }
}


#' Heatmap of a Groupwise Object 
#' 
#' Plots a heatmap, that shows the performance of cascades of a Groupwise object.
#' 
#' @param x
#' A Groupwise object as it is returned by \code{\link{groupwise}}-function. 
#' @param class.sort
#' The classes can be sorted either according to the first cascade in the Groupwise object (''), 
#' or based on the class frequency ('max').
#' @param row.sort
#' The cascade can be sorted either based on the maximal-minimal class-wise sensitivity ('sens') or 
#' according the class frequency.
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
#' No return value, called to generate the heatmap plot of the Groupwise Object.
#' 
#' @details 
#' This function plots a heatmap with the cascades of the Groupwise object in the rows 
#' and all classes present in any of the cascades in the columns. 
#' The colors indicate whether a given class is present in the corresponding cascade and with which sensitivity.
#' Internally converts the Groupwise object to a Subcascades object and plots the corresponding heatmap.
#' 
#' @seealso \code{\link{groupwise}}, \code{\link{subcascades}}, \code{\link{plot.Subcascades}}
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' subc <- subcascades(predMap,thresh=0.7,size=c(3,4))
#' groupwise <- groupwise(subc)
#' 
#' plot(groupwise,row.sort='max')
#' 
plot.Groupwise <- function(x       = NULL,
               class.sort = '',
               row.sort = 'sens',
               main    = 'groupwise',
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
               ...) {
  subc <- as.subcascades(x)
  print(subc)
  #call plot method of subcascades
  plot(subc, class.sort = class.sort, row.sort = row.sort,
       main = main, xlab = xlab, ylab = ylab, digits = digits, ignore = ignore, 
       casc.colors = casc.colors, na.color = na.color, color.key = color.key,
       las = las, cex = cex, cex.lab = cex.lab, srt = srt)
}

#' Heatmap of a PredictionMap Object
#' 
#' Plots a heatmap, that shows the predictions of a PredictionMap object and 
#' the real labels in a cross-validation or reclassification experiment.
#' 
#' @param x
#' A PredictonMap object as it is returned by \code{\link{predictionMap}}-function. 
#' @param xlab
#' A title for the x axis (see \code{\link{plot}}).
#' @param ylab
#' A title for the y axis (see \code{\link{plot}}).
#' @param main
#' See \code{\link{plot}}.
#' @param las
#' See \code{\link{par}}.
#' @param cex
#' See \code{\link{par}}.
#' @param cex.lab
#' See \code{\link{par}}.
#' @param srt
#' Angle used to rotate the strings of the x-axis and y-axis labels (see \code{\link{par}}).
#' @param label.colors
#' A vector of the color for the class labels. 
#' If NULL, automatically use rainbow color scheme.
#' @param plot.sampleIDs
#' Specifices if the sample IDs should be plotted along the x axis (TRUE or FALSE).
#' @param plot.cv.runs,
#' Specifices if the cross-validation runs should be plotted (TRUE or FALSE). 
#' Cross-validation runs are visually separated by straight lines.
#' @param plot.class.labels
#' Specificies if the numerical class labels should additionally plotted (TRUE or FALSE).
#' @param ... Further arguments passed from other methods.
#' 
#' @return 
#' No return value, called to a heatmap plot of the predictionMap Object.
#' 
#' @details 
#' This function plots a heatmap with color-decoded predictions made by the 
#' specified classifier.
#' Here, the rows indicate the different binary base classifiers and the columns the samples 
#' in the specified resampling experiment (reclassification or cross-validation).
#' Labels are visualized in the top row and decoded color-wise. 
#' 
#' @seealso \code{\link{predictionMap}}
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
#' predMap <- predictionMap(data, labels, foldList = foldList, 
#'                         classifier = tunePareto.svm(), kernel='linear')
#' 
#' plot(predMap)
#' 
plot.PredictionMap <- function(x=NULL,
                                xlab = 'samples',
                                ylab = 'base classifiers',
                                main = "Prediction map",
                                las=1,
                                srt = 30,
                                cex = 1,
                                cex.lab = 1,
                                label.colors=NULL,
                                plot.sampleIDs=FALSE,
                                plot.cv.runs=TRUE,
                                plot.class.labels=TRUE,
                                ...) {
  
  
  #################################################
  ##
  ## Check parameter 'x'
  
  if(is.null(x))
    stop(errorStrings('predictionMapMissing'))
  
  if(!inherits(x, 'PredictionMap'))
    stop(errorStrings('predictionMap'))
  
  #################################################
  ##
  ## Prepare
  
  #predMap <- x
  meta <- x$meta
  pred <- x$pred
  
  classes <- unique(meta["label",])
  numClasses <- length(classes)
  na.color <- c('#ffffff')
  
  #################################################
  ##
  ## Filter out -1 entries and create resultMatrix
  
  pred <- pred[-(which(apply(pred,1,mean)==-1)),,drop = FALSE]
  
  numPred<-ncol(pred)
  numBase<-nrow(pred)
  
  resultMatrix <- rbind(labels = meta["label",],rep(NA,numPred),pred)
  colnames(resultMatrix) <- meta["sample",]
  
  resultMatrix <- resultMatrix[numBase:1,,drop = FALSE]
  
  #################################################
  ##
  ## Preparation
  
  ylim <- c(0.5, numBase+0.5)
  
  colnms <- colnames(resultMatrix)
  rownms <- rownames(resultMatrix)
  
  if(is.null(label.colors)) #automatically generate label colors
    label.colors <- rainbow(numClasses)
  
  ###################################################
  ###
  ### plot background
  
  plot(   0,0,
          type = 'n',
          xlim = c(0.5,numPred+0.5),
          ylim = ylim,
          main = main,
          xlab = xlab,
          ylab = ylab,
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
  
  # plot sampleIDs at bottom?
  if(plot.sampleIDs)
    axis(1, seq_len(numPred), colnms, las=las, ...)
  
  axis(2, seq_len(numBase), rownms, las=las, ...)
  
  ###################################################
  ###
  ### plot extended confusion table
  
  coordinates <- expand.grid(y=seq_len(numBase),x=seq_len(numPred))
  
  colors <- apply(coordinates,1,function(y){
    val <- resultMatrix[y[1],y[2]]
    if(is.na(val))
      return(NA)
    
    label.colors[val+1]
  })
  
  rect(   coordinates[,'x']-0.5,
          coordinates[,'y']-0.5,
          coordinates[,'x']+0.5,
          coordinates[,'y']+0.5,
          col = colors,
          border = NA)
  
  ###################################################
  ###
  ### plot class label numbers and cross-validation runs (if specified by user)
  
  one.run  <- all(meta["run",]==1)
  one.fold <- all(meta["fold",]==1)
  reclass  <- one.run & one.fold
  loocv    <- one.run & all(meta["fold",]==meta["sample",])
  other.cv <- !(reclass || loocv)
  
  #where to plot "Run X" text
  if(other.cv){
    ind.run.flips <- which(meta["run",1:(numPred-1)]-meta["run",2:numPred] != 0) + 1
    tmp <- c(ind.run.flips,numPred+1)
    run.indices <- tmp - diff(c(0,tmp))/2
  }else{
    label.flips <- which(meta["label",1:(numPred-1)]-meta["label",2:numPred] != 0) + 1
    tmp <- c(label.flips,numPred+1)
    label.indices <- tmp - diff(c(0,tmp))/2
  }
  
  if(plot.class.labels){
    if(other.cv) #cross-validation
    {
      text(x=run.indices,y=rep(numBase,length(run.indices)),labels=paste(rep(classes,each=length(unique(meta["run",])),sep="")))
    }else{ #reclassification or leave-one-out
      text(x=label.indices,y=rep(numBase,length(label.indices)),labels=paste(classes,sep=""))
    }
  }
  
  if(plot.cv.runs && other.cv) {
    abline(v=ind.run.flips-0.5)
    text(x=run.indices,y=rep(numBase-1,length(run.indices)),labels=paste("Run ",meta["run",c(1,ind.run.flips)],sep=""))
  }
}