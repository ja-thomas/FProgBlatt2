#'Predict values of a logitreg object
#'
#'Predict values of a logitreg object or for newdata with the same dimensions as
#'the original data.
#'
#'@param object logitreg. An object of class logitreg.
#'@param newdata data.frame. Optional, additional data to predict. Has to have 
#'the same dimensions as the data of the logitreg object. Tries to convert the 
#'object to a data.frame if it is not already one.
#'@author Janek Thomas, Philipp Roesch
#'@return Vector of predicted probabilities.
#'@export  
predict.logitreg <- function(object, newdata){
  if(missing(newdata)){
    newdata <- object$data$design
  }
  else{
    if(!is.data.frame(newdata)){
      tryCatch(eval(newdata <- as.data.frame(newdata)), error = function(e){
        stop("<newdata> could not be converted to a dataframe")
      })
      warning("<newdata> converted to data.frame")
    }
    
    if(ncol(newdata) != length(object$coefficients) -1){
      stop("dimensions of model and new data do not fit")
    }
    
    newdata <- model.matrix(~. , newdata)
  }
  logit_link(newdata %*% object$coefficients)
}