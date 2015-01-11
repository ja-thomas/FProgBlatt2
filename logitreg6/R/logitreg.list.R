#'Calculate a logistic regression for list design input
#'
#'Calculate a logistic regression for given data. Uses 
#'\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS) 
#'algorithm as default for parameter estimation 
#'@param design list. Design matrix containing the observations, first
#'list should be constant 1 as intercept
#'@param response numeric vector. Response vector of 1s and 0s, should have the 
#'same length as the number of rows in design.
#'@param ... Further parameters passed to \code{\link[stats]{optim}}. 
#'@author Janek Thomas, Philipp Rösch, Steffen Fohr
#'@return A list with estimated coefficients, fitted propabilities and 
#'original data
#'@encoding UTF-8
#'@export
#'@seealso \code{\link{logitreg}} 
logitreg.list <- function(design, response, ...){
  
  # input checking
  design_list_length <- length(design)
  inlist_length <- rep(NA, times = design_list_length)
  for (i in seq_len(design_list_length)){
    if (is.matrix(design[[i]]) | is.data.frame(design[[i]])) {
      inlist_length[i] <- nrow(design[[i]])
    }
    if(is.vector(design[[i]])) {
      inlist_length[i] <- length(design[[i]])
    }
  }
  if((min(inlist_length) == max(inlist_length)) == FALSE) {
    stop("number of rows of list input need to have to same length")
  }
  # end of input checking
  
  design_matrix <- do.call(cbind, design)
  
  if(!all(design_matrix[, 1] == 1)){
   stop("intercept of <design> can only be an one vector") 
  }
  
  logitreg.default(design = design_matrix,
                   response = response, ...)
} 