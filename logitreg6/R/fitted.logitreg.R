fitted.logitreg <- function(object){
  fitted_probabilities <- object$fitted[ , 1]
  names(fitted_probabilities) <- rownames(object$fitted)
  fitted_probabilities
}