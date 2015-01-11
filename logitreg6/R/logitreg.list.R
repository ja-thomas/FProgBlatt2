#'Calculate a logistic regression for list design input
#'
#'Calculate a logistic regression for given data. Uses 
#'\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS) 
#'algorithm as default for parameter estimation 
#'@param design list. Design matrix containing the observations, first
#'column should be constant 1 as intercept
#'@param response numeric vector. Response vector of 1s and 0s, should have the 
#'same length as the number of rows in design.
#'#'@author Janek Thomas, Philipp Roesch
#'@return A list with estimated coefficients, fitted propabilities and 
#'original data
#'@export
#'@seealso The default method \code{\link{logitreg.default}} and  \code{\link{logitreg.list}} for lists

logitreg.list <- function(design, response, ...){
  
  design_matrix <- do.call(cbind, design)
  # checks for same vector lengths
  
  if(!all(design_matrix[, 1] == 1)){
   stop("intercept of <design> can only be an one vector") 
  }
  
  logitreg.default(design = design_matrix,
                   response = response, ...)
} 