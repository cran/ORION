library(ORION)

test_that("Full pipeline with data projection",{
  
  ### load file 
  file <- system.file("extdata", "artificial_2D_alternatives_sd_0.2-0.3.RData", package = "ORION")
  load(file)
  
  ## project the data
  projectedData <- projectData(dataset=dataset, comb=c(0,5)) 
  
  ## generate CV fold lists
  foldList <- generateCVRuns(labels = dataset$labs,
                             ntimes = 10,
                             nfold = 10)
  
  ### test-predictionMap tunePareto svm
  predMap <- predictionMap(data=t(projectedData$data),
                           labels=projectedData$labs,
                           foldList = foldList,
                           parallel = FALSE,
                           learner = "tunePareto",
                           classifier = tunePareto.svm(), 
                           kernel='linear') 
  
  ## test subcascades - keep longest cascades
  sub <- subcascades(predictionMap=predMap, sets = NULL, thresh=1, size=NA, numSol=1000)[1]
  class(sub) <- "Subcascades"
  
  sets <- rownames(sub[[1]])
  
  simplifiedSub <- list()
  
  while(!is.null(sub)){
    head <- sub[1]
    sets <- rownames(head[[1]])
    
    simplifiedSub <- c(simplifiedSub, head)
    
    sub <- dropSets(subcascades=sub, sets=sets, ordered = TRUE,  neighborhood = "indirect", type = "any", direction="super")
  }
  
  class(simplifiedSub) <- "Subcascades"
  
  ### generate dot string graph
  dot_str <-generateGraphs(subcascade=simplifiedSub, useOldLabels=F, categoricalLabels=NULL)
  #g <-  generateGraphs(subcascade=subArt1_0, useOldLabels=T, numericLabels=0:10, categoricalLabels=letters[1:10])   # with categorical lables
  #dot(g)
  expect_type(dot_str, "character")
})



