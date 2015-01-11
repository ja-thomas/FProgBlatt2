#'Calculate a logistic regression
#'
#'Calculate a logistic regression for given data. Uses 
#'\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS) 
#'algorithm as default for parameter estimation 
#'@param design numeric matrix. Design matrix containing the observations, first
#'column should be constant 1 as intercept
#'@param response numeric vector. Response vector of 1s and 0s, should have the 
#'same length as the number of rows in design.
#'@param method character. optimization method, one of "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
#' "Brent".
#'@param ... Further parameters passed to \code{\link[stats]{optim}}.  
#'@author Janek Thomas, Philipp Rösch, Steffen Fohr
#'@return A list with estimated coefficients, fitted propabilities and 
#'original data
#'@encoding UTF-8
#'@export
logitreg <- function(design, response, method = "BFGS", ...){
    
  method <- match.arg(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B",
                                "SANN", "Brent"))
  
  if(!is.matrix(design)){
    if(is.data.frame(design)){
      design <- as.matrix(design)
      warning("<design> converted from data.frame to matrix")
    }
    else{
      stop("<design> is not a matrix")
    }  
  }
  
  if(!all(is.numeric(design))){
    stop("non numeric values in <design>")
  }
  
  if(!is.vector(response)){
    stop("<response> is not a vector")
  }
  
  if(!all(response %in% c(1,0))){
    stop("<response> can only have values of 1 and 0")
  }
  
  if(!length(response) == nrow(design)){
    stop("Dimensions of <response> and <design> do not fit")
  }
  
  initial_coefficients <- rep(0.1, times = ncol(design))
  
  optimization_result <- optim(par = initial_coefficients, fn = neg_loglik, 
                               gr = neg_loglik_deriv, response = response, 
                               design = design, method = method, ...)
  
  coefficients <- optimization_result$par
  fitted <- logit_link(design %*% coefficients)
  data <- list(design = design, response = response)
  
  list(coefficients = coefficients, fitted = fitted, data = data)
}