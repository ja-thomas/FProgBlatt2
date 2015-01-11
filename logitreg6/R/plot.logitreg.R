#'Plot a ROC curve for a logitreg model.
#'
#'Plot a ROC (reciever operating characteristic) curve for a logitreg model.
#'See \code{\link[ROCR]{plot.performance}} for further information.
#'
#'@param object logitreg. An object of class logitreg.
#'@param ... Additional parameters passed to the plot function, see 
#'\code{\link[ROCR]{plot.performance}} for details.
#'@author Janek Thomas, Philipp Roesch
#'@import ROCR
plot.logitreg <- function(object, ...){

  prediction_object <- prediction(object$fitted, object$data$response)
  true_and_fals_positive_rates <- performance(prediction_object,"tpr","fpr")
  plot(true_and_fals_positive_rates, ...)
}