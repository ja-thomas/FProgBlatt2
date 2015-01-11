#'Get the fitted values a logitreg model.
#'
#'Get the fitted values a logitreg model.
#'
#'@param object logitreg. An object of class logitreg.
#'@author Janek Thomas, Philipp Roesch
fitted.logitreg <- function(object){
  fitted_probabilities <- object$fitted[ , 1]
  names(fitted_probabilities) <- rownames(object$fitted)
  fitted_probabilities
}