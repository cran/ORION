#' Extended Confusion Table Plot
#' 
#' Plots an extended confusion table.
#' 
#' @inheritParams summarySubcascades
#' @inheritParams plot.Conf
#' @param x
#' A ConfusionTable object as it is returned by \code{\link{confusion.table}}-function. 
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
#' @seealso \code{\link{confusion.table}}, \code{\link{plot.Subcascades}}, \code{\link{plot.Conf}}
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
#' conf.table <- confusion.table(genMap,cascade='0>1>3>4',other.classes = 'all')
#' 
#' plot(conf.table)
#' 
#' 



plot.ConfusionTable <- function( x       = NULL,
                                 classNames           = NULL,
                                 main    = 'extended confusion table',
                                 xlab    = 'original class labels',
                                 ylab    = 'predictions',
                                 cascLab = 'cascade',
                                 otherLab= 'other classes',
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
{  confusionTable <- x
    #################################################
    ##
    ## Check parameter 'confusionTable'
    
    if(is.null(confusionTable))
        stop(errorStrings('confusionTableMissing'))
    
    if(!inherits(confusionTable, 'ConfusionTable'))
        stop(errorStrings('confusionTable'))
    
    #################################################
    ##
    ## Preparation
    
    exist.foreign.classes <- ncol(confusionTable)>nrow(confusionTable)
    
    numColors <- 101
    cascRamp  <- colorRampPalette(casc.colors)(numColors)
    otherRamp <- colorRampPalette(other.colors)(numColors)
    
    xbox <- c(-0.5,-0.5,0.5,0.5)
    ybox <- c(-0.5,0.5,0.5,-0.5)
    ylim <- c(0.5, nrow(confusionTable)+0.5)
    
    colnms <- as.numeric(sapply(colnames(confusionTable), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
    rownms <- as.numeric(sapply(rownames(confusionTable), function(z){strsplit(z,'.',fixed=TRUE)[[1]][2]}))
    
    if(!is.null(classNames))
    {   
        if(max(c(colnms+1,rownms+1))>length(classNames))
            stop(errorStrings('classNames'))
        
        colnms <- classNames[colnms+1]
        rownms <- classNames[rownms+1]
    }
    
    extension <- 0
    a1.label <- c()
    a1.pos <- c()
    a3.label <- c()
    a3.pos <- c()
    if(color.key)
    {
       if(exist.foreign.classes)
        {
            extension <- 4
            a1.label <- c('casc','other')
            a1.pos  <- ncol(confusionTable)+c(2,4)
            
            a3.label <- 'color keys'
            a3.pos <- ncol(confusionTable)+3
        }else{
            extension <- 2
            a1.label <- 'casc'
            a1.pos  <- ncol(confusionTable)+2
            
            a3.label <- 'color key'
            a3.pos <- ncol(confusionTable)+2
        }
    }
    
    ###################################################
    ###
    ### plot background
    
    plot(   0,0,
            type = 'n',
            xlim = c(0.5,ncol(confusionTable)+0.5+extension),
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
    
    axis(1, seq_len(ncol(confusionTable)), colnms, las=las, ...)
    axis(2, seq_len(nrow(confusionTable)), rownms, las=las, ...)
    
    if(!exist.foreign.classes)
    {
        axis(3, c(nrow(confusionTable)/2), cascLab, line = -1, las=1, cex = cex.lab, ...)
    }else{
        axis(3, c(nrow(confusionTable)/2, (nrow(confusionTable)+1+ncol(confusionTable))/2), c(cascLab,otherLab), line = -1,las=1, ...)
    }
    
    ###################################################
    ###
    ### plot color key
    
    if(color.key)
    {
        axis(1, a1.pos[1], a1.label[1], las=las, ...)
        axis(3, a3.pos, a3.label, line = -1, las=1, tick = FALSE, ...)
        text(x=a1.pos[1]-1, y=mean(ylim),labels = 'confusion', srt = 90, cex = cex.lab)
        
        if(exist.foreign.classes)
        {
            axis(1, a1.pos[2], a1.label[2], las=1, ...)
            text(x=a1.pos[2]-1, y=mean(ylim),labels = 'external rates', srt = 90, cex = cex.lab)
        }
        
        axis(4, ylim, c(0,1), las=1, ...)

        color.bounds <- seq(ylim[1], ylim[2], by = (ylim[2]-ylim[1])/numColors)
        
        xcoord   <- rep(a1.pos[1],numColors)
        ycoord.l <- color.bounds[1:numColors]
        ycoord.u <- color.bounds[2:(numColors+1)]
        colors   <- cascRamp
        
        polygon( x   = xbox+a1.pos[1],
                 y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
                 col = 'white',
                 lwd = 2)
        
        if(exist.foreign.classes)
        {
            xcoord   <- c(xcoord,rep(a1.pos[2],numColors))
            ycoord.l <- rep(ycoord.l,2)
            ycoord.u <- rep(ycoord.u,2)
            colors   <- c(colors,otherRamp)
            
            polygon( x   = xbox+a1.pos[2],
                     y   = c(ylim[1],ylim[2],ylim[2],ylim[1]),
                     col = 'white',
                     lwd = 2)
        }
        
        rect(   xcoord-0.5,
                ycoord.l,
                xcoord+0.5,
                ycoord.u,
                col = colors,
                border = NA)
    }
    
    ###################################################
    ###
    ### plot extended confusion table
   
    coordinates <- expand.grid(y=seq_len(nrow(confusionTable)),x=seq_len(ncol(confusionTable)))
    
    colors <- apply(coordinates,1,function(x){
       i <- x[1]
       j <- x[2]
       
       if(j <= nrow(confusionTable))
       {
           val <- round(confusionTable[i,j], digits = digits)
           return(cascRamp[val*100+1])
       }else{
           val <- round(confusionTable[i,j], digits = digits)
           return(otherRamp[val*100+1])
       }
    })
    
    texts <- apply(coordinates,1,function(x){
        i <- x[1]
        j <- x[2]
        
        return(round(confusionTable[i,j], digits = digits))
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
    ### plot separator
    
    if(exist.foreign.classes)
        abline(v = nrow(confusionTable)+0.5, col = colSep, lwd=4)
    
}
