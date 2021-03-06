#'calculate the logistic link function
#'
#'calculate the logistic link function for one or more observations
#'@param x numeric vector. Values to calculate the logistic link function for
#'@author Janek Thomas, Philipp Rösch, Steffen Fohr
#'@encoding UTF-8
logit_link <- function(x){
  1 / (1 + exp(-1 * x))
}