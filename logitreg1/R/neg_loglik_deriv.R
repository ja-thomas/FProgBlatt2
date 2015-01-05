#'Calculate the gradient negative loglikelihood for logistic regression
#'
#'Calculate the gradient negative loglikelihood for logistic regression.
#'Used for optimization of the likelihood.
#'@inheritParams neg_loglik
#'@author Janek Thomas, Philipp Roesch
#'@export
neg_loglik_deriv <- function(coefficients, response, design){

  -1 * t(response - logit_link(design %*% coefficients)) %*% design

}
