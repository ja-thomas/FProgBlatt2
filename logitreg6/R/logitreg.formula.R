#'Calculate a logistic regression for formula input
#'
#'Calculate a logistic regression for given data. Uses 
#'\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS) 
#'algorithm as default for parameter estimation 
#'@param design an object of class "formula": a symbolic description of the 
#'model to be fitted.
#'@param data an optional data frame, containing the variables in the model. 
#'If not found in data, the variables are taken from the parent environment.
#'@param ... Further parameters passed to \code{\link[stats]{optim}}.  
#'@author Janek Thomas, Philipp Rösch, Steffen Fohr
#'@return A list with estimated coefficients, fitted propabilities and 
#'original data
#'@encoding UTF-8
#'@export
#'@seealso \code{\link{logitreg}}
logitreg.formula <- function(design, data = NULL, ...){
  
  # input checking
  if(grepl("- 1", toString(eval(quote(design))))){
    stop("formula of <design> cannot contain '- 1'. Model fitting without 
         intercept is not supported")
  }
  # end input checking
  
  if(is.null(data)){
    model_frame <- eval(bquote(model.frame(.(design))), parent.frame())
    
    model_frame_length <- length(model_frame)
    for (i in seq_len(model_frame_length)){
      if(is.factor(model_frame[,i])){
        levels(model_frame[,i]) <- c(0,1)
        model_frame[,i] <- as.numeric(as.character(model_frame[,i]))
      }
    }
  
  }
  else{
    model_frame <- model.frame(design, data) 
  }
  
  design_matrix <- cbind(intercept=1, as.matrix(model_frame[ ,-1]))
  
  logitreg.default(design = design_matrix,
             response = model_frame[, 1],
             ...)
}