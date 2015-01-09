logitreg.formula <- function(design, data = NULL, ...){
  if(is.null(data)){
    model_frame <- eval(bquote(model.frame(.(design))), parent.frame())
    
    if(is.factor(model_frame[,1])){
      levels(model_frame[,1]) <- c(0,1)
      model_frame[,1] <- as.numeric(model_frame[,1])
    }
    
    
  }
  else{
    model_frame <- model.frame(design, data) 
  }
  

  logitreg.default(design = as.matrix(model_frame[ ,-1]),
             response = model_frame[, 1],
             ...)
}