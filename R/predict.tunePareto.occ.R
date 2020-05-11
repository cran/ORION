predict.tunePareto.occ <- function(object, newdata,...)
{
  ##################################################################
  #### Checks
  
  if (missing(newdata))
    stop('Please supply \'newdata\' !')
  
  ####################################################################
  ####
  #### Rountine
  
  base.predictions <- sapply(object$base.classifiers, function(bC){predict(bC,newdata,...)})
  
  if(!is.matrix(base.predictions))
    base.predictions <- matrix( base.predictions,
                                nrow = nrow(newdata),
                                ncol = length(object$base.classifiers))
  
  numClasses <- length(object$base.classifiers)+1
  
  ensemble.predictions <- apply(base.predictions,1,function(x){
    index <- which(x==object$class.order[-numClasses])
    
    if(length(index)==0)
    {
      object$class.order[numClasses]
    }else{
      object$class.order[index[1]]
    }
  })
  
  ##################################################################
  ####
  #### Result
  
  return(ensemble.predictions)
}

