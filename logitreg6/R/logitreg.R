#'Calculate a logistic regression
#'
#'Generic function to calculate a logistic regression for given data. Uses 
#'\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS) 
#'algorithm as default for parameter estimation.
#'@param design numeric matrix or list containing the observations. First
#'column/list should be constant 1 as intercept. resonse need to be specifies. 
#'Alternativly an object of class "formula".
#'@param ... Further parameters passed to other logitreg inout and 
#'\code{\link[stats]{optim}}.  
#'@author Janek Thomas, Philipp Rösch
#'@return A list with estimated coefficients, fitted propabilities and 
#'original data.
#'@encoding UTF-8
#'@export
#'@seealso The default method \code{\link{logitreg.default}} for matrixes 
#'(and data frames), \code{\link{logitreg.formula}} for formula, and  
#'\code{\link{logitreg.list}} for lists
logitreg <- function(design, ...) UseMethod("logitreg")