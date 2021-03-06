% Generated by roxygen2 (4.0.2): do not edit by hand
\encoding{UTF-8}
\name{logitreg.formula}
\alias{logitreg.formula}
\title{Calculate a logistic regression for formula input}
\usage{
\method{logitreg}{formula}(design, data = NULL, ...)
}
\arguments{
\item{design}{an object of class "formula": a symbolic description of the
model to be fitted.}

\item{data}{an optional data frame, containing the variables in the model.
If not found in data, the variables are taken from the parent environment.}

\item{...}{Further parameters passed to \code{\link[stats]{optim}}.}
}
\value{
A list with estimated coefficients, fitted propabilities and
original data
}
\description{
Calculate a logistic regression for given data. Uses
\code{\link[stats]{optim}} with the Broyden-Fletcher-Goldfarb-Shanno (BFGS)
algorithm as default for parameter estimation
}
\author{
Janek Thomas, Philipp Rösch
}
\seealso{
\code{\link{logitreg}}
}

