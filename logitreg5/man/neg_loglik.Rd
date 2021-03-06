% Generated by roxygen2 (4.0.2): do not edit by hand
\encoding{UTF-8}
\name{neg_loglik}
\alias{neg_loglik}
\title{Calculate the negative loglikelihood of a logistic regression}
\usage{
neg_loglik(coefficients, response, design)
}
\arguments{
\item{coefficients}{a vector. The coefficients of the model, length must be
the same as the number of columns in the design matrix}

\item{response}{a vector. The resonse of the data, length must be the same as
the number of rows in the design matrix}

\item{design}{numeric matrix. The data used to fit the loglikelihood}
}
\description{
Return the negative loglikelihood for a data matrix with response
coefficients.
}
\author{
Janek Thomas, Philipp Rösch
}

