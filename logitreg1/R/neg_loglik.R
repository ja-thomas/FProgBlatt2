#'Calculate the negative loglikelihood of a logistic regression
#'
#'Return the negative loglikelihood for a data matrix with response
#'coefficients.
#'@param coefficients a vector. The coefficients of the model, length must be 
#'the same as the number of columns in the design matrix
#'@param response a vector. The resonse of the data, length must be the same as 
#'the number of rows in the design matrix
#'@param design numeric matrix. The data used to fit the loglikelihood
#'@author Janek Thomas
#'@export
neg_loglik <- function(coefficients, response, design){
    
  nu <- design %*% coefficients
  
  -1 * sum(response * log(logit_link(nu)) +
             (1 - response) * log(1 - logit_link(nu)))
  
}

