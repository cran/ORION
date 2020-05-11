#' Base Classifier Performance Heatmap
#' 
#' Plots a heatmap that shows base classifier performance.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Subcascades
#' @param x
#' A Conf object as it is returned by \code{\link{gen.conf}}-function. 
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
#' 
#' 
#' 
#' @param ... Further arguments passed from other methods.
#' 
#' @details 
#' This function plots a heatmap of the base classifier performance.
#' 
#' @seealso \code{\link{gen.conf}}, \code{\link{plot.Subcascades}}, \code{\link{plot.ConfusionTable}}
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
#' genMap <- gen.predictionMap(data, labels, foldList = foldList, 
#' classifier = tunePareto.svm(), kernel='linear')
#' # generate Subcascades object
#' conf <- gen.conf(genMap)
#' 
#' plot(conf,symmetric=TRUE)
#' 
#' 

plot.Conf <- function(  x         = NULL,
                        classNames   = NULL,
                        symmetric    = FALSE,
                        main         = 'summary of base classifiers',
                        xlab         = 'second class',
                        ylab         = 'first class',
                        digits       = 3,
                        ignore       = 0,
                        first.colors  = c('#f5f5f5','#8c510a'),
                        second.colors = c('#f5f5f5','#01665e'),
                        na.color     = c('yellow'),
                        las=1,
                        color.key = TRUE,
                        cex = 1,
                        cex.lab = 1,
                        ...)
{   conf <- x
    #################################################
    ##
    ## Check parameter 'conf'
    
    if(is.null(conf))
        stop(errorStrings('confMissing'))
    
    if(!inherits(conf, 'Conf'))
        stop(errorStrings('conf'))
    
    numClasses <- sqrt(length(conf$fC))
    
    ###########################################################
    ###
    ### Extract sensitivities of first classes
    
    firstClass.matrix <- sapply((1:numClasses)-1, function(i){
        sapply((1:numClasses)-1, function(j){
            if(i==j)
            {
                return(NA)
            }else{
                return(conf$fC[paste('[',i,'vs',j,']',sep = ''),])
            }
        })
    })

    colnames(firstClass.matrix) <- paste('fC.',(1:numClasses)-1, sep = '')
    rownames(firstClass.matrix) <- paste('sC.',(1:numClasses)-1, sep = '')

    ###########################################################
    ###
    ### Extract sensitivities of second classes
    
    if(!symmetric)
    {
        secondClass.matrix <- sapply((1:numClasses)-1, function(i){
            sapply((1:numClasses)-1, function(j){
                if(i==j)
                {
                    return(NA)
                }else{
                    return(conf$sC[paste('[',i,'vs',j,']',sep = ''),paste('TN',j,sep = '')])
                }
            })
        })

        colnames(secondClass.matrix) <- paste('fC.',(1:numClasses)-1, sep = '')
        rownames(secondClass.matrix) <- paste('sC.',(1:numClasses)-1, sep = '')
    }
    ###########################################################
    ###
    ### Preparation plot
    
    xbox <- c(-0.5,-0.5,0.5,0.5)
    ybox <- c(-0.5,0.5,0.5,-0.5)
    ylim <- c(0.5, numClasses+0.5)
    
    colnms <- as.numeric(sapply(colnames(firstClass.matrix), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
    rownms <- as.numeric(sapply(rownames(firstClass.matrix), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
    
    if(!is.null(classNames))
    {
      if(max(c(colnms+1,rownms+1))>length(classNames))
        stop(errorStrings('classNames'))
      
        colnms <- classNames[colnms+1]
        rownms <- classNames[rownms+1]
    }
    
    numColors  <- 101
    firstRamp  <- colorRampPalette(first.colors)(numColors)
    
    if(symmetric)
    {
        numCol <- numClasses
    }else{
        secondRamp <- colorRampPalette(second.colors)(numColors)
        numCol <- 2*numClasses+2
    }
    
    if(color.key)
    {

        if(symmetric)
        {
            extension<- 2
            a1.label <- c('first')
            a1.pos   <- numCol+2

            a3.label <- 'color key'
            a3.pos   <- numCol+2
        }else{
            extension<- 4
            a1.label <- c('first','second')
            a1.pos   <- numCol+c(2,4)
        
            a3.label <- 'color keys'
            a3.pos   <- numCol+3
        }
    }else{
        extension <- 0
        a1.label <- c()
        a1.pos <- c()
 
        a3.label <- c()
        a3.pos <- c()
    }
    
    ###################################################
    ###
    ### plot background
    
    plot(   0,0,
            type = 'n',
            xlim = c(0.5,numCol+0.5+extension),
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
            
    axis(1, 1:numClasses, colnms, las=las, ...)
    axis(2, 1:numClasses, rownms, las=las, ...)
    
    if(!symmetric)
    {
        axis(1, (1:numClasses)+numClasses+2, colnms, las=las, ...)
        
        axis(3, numClasses/2,       'sensitivity for first class',   las=1, ...)
        axis(3, numClasses*(1.5)+2, 'sensitivity for second class', las=1, ...)
        
        text(numClasses+1.75,1:numClasses,colnms,cex = cex)
        text(numClasses+2.3,1:numClasses,rep('-',numClasses),cex = cex)
    }else{
        axis(3, ceiling(numClasses/2), 'symmetric base classifier',   las=1, tick = F, line=-1, ...)
    }
    
  
    ###################################################
    ###
    ### plot color key
  
    if(color.key)
    {
        axis(1, a1.pos[1], a1.label[1], las=las, ...)
        axis(3, a3.pos, a3.label, line = -1, las=1, tick = FALSE, ...)
        
        text(x=a1.pos[1]-1, y=mean(ylim),labels = 'sensitivity for first class', srt = 90, cex = cex.lab)
        
        axis(4, ylim, c(0,1), las=1, ...)
        
        color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
        
        polygon( x   = xbox+a1.pos[1],
                 y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
                 col = 'white',
                 lwd = 2)
   
        xcoord <- rep(a1.pos[1],numColors)
        ramp   <- firstRamp
   
        if(!symmetric)
        {
            axis(1, a1.pos[2], a1.label[2], las=1, ...)
            text(x=a1.pos[2]-1, y=mean(ylim),labels = 'sensitivity for second class', srt = 90, cex = cex.lab)
        
            polygon( x   = xbox+a1.pos[2],
                     y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
                     col = 'white',
                     lwd = 2)

            xcoord <- c(xcoord,rep(a1.pos[2],numColors))
            ramp   <- c(ramp, secondRamp)
        }
   
        rect(   xcoord-0.5,
                rep(color.bounds[1:numColors],2),
                xcoord+0.5,
                rep(color.bounds[2:(numColors+1)],2),
                col = ramp,
                border = NA)
    }
    ###################################################
    ###
    ### plot table
    
    if(symmetric)
    {
        coordinates <- expand.grid(1:numClasses,1:numClasses)
        colors <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
                return(na.color)
             
             val <- round(firstClass.matrix[i,j], digits = digits)
             return(firstRamp[val*100+1])
        })
        
        texts <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
                return(-1)
            
            return(round(firstClass.matrix[i,j], digits = digits))
        })
        
        texts[texts<=ignore] <- NA
        
        rect(   coordinates[,1]-0.5,
                coordinates[,2]-0.5,
                coordinates[,1]+0.5,
                coordinates[,2]+0.5,
                col = colors,
                border = 'black')

        text(coordinates[,1],coordinates[,2],texts,cex = cex)
    }else{
        coordinates <- expand.grid(1:numClasses,1:numClasses)
        colors.fC <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
                return(na.color)
                
            val <- round(firstClass.matrix[i,j], digits = digits)
            
            return(firstRamp[val*100+1])
        })
        
        colors.sC <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
            return(na.color)
            
            val <- round(secondClass.matrix[i,j], digits = digits)
            
            return(secondRamp[val*100+1])
        })
        
        texts.fC <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
                return(-1)
            
            return(round(firstClass.matrix[i,j], digits = digits))
        })
        texts.fC[texts.fC<=ignore] <- NA
        
        texts.sC <- apply(coordinates,1,function(x){
            i <- x[1]
            j <- x[2]
            if(i==j)
            return(-1)
            
            return(round(secondClass.matrix[i,j], digits = digits))
        })
        texts.sC[texts.sC<=ignore] <- NA

        xcoord <- c(coordinates[,2],coordinates[,2]+numClasses+2)
        ycoord <- rep(coordinates[,1],2)
        
        rect(   xcoord-0.5,
                ycoord-0.5,
                xcoord+0.5,
                ycoord+0.5,
                col = c(colors.fC,colors.sC),
        border = 'black')
        
        text(xcoord, ycoord ,c(texts.fC,texts.sC),cex = cex)
    }
}
