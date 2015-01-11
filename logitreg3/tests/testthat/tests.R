library(testthat)

source("http://www.statistik.lmu.de/~scheipl/downloads/fortprog/simulate_logitreg.R")

data <- simulate_logitreg(seed = 1337)
data_small <- simulate_logitreg(n = 150, seed = 1337 )
data_more_covariables <- simulate_logitreg(q_numeric = 10, seed = 1337)

get_coefficients_from_glm <- function(data){
  unname(glm(data$y ~ data$x[,-1], 
             family = binomial(link = logit))$coefficients) 
}

test_that("Different sizes of data:", {
expect_equal(get_coefficients_from_glm(data), 
             logitreg(data$x, data$y)$coefficients, tolerance = 0.0009)
expect_equal(get_coefficients_from_glm(data_small), 
             logitreg(data_small$x, data_small$y)$coefficients, 
             tolerance = 0.0009)
expect_equal(get_coefficients_from_glm(data_more_covariables), 
             logitreg(data_more_covariables$x, 
                      data_more_covariables$y)$coefficients, tolerance = 0.0009)
})

test_that("Different optim parameters",{
  expect_equal(get_coefficients_from_glm(data), 
               logitreg(data$x, data$y, method = "Nelder-Mead")$coefficients, 
               tolerance = 0.0009)
  expect_equal(get_coefficients_from_glm(data), 
               logitreg(data$x, data$y, method = "CG")$coefficients, 
               tolerance = 0.0009)
  expect_equal(get_coefficients_from_glm(data), 
               logitreg(data$x, data$y, 
                        control = list(ndeps = 1e-2))$coefficients, 
               tolerance = 0.0009)
  expect_equal(get_coefficients_from_glm(data), 
               logitreg(data$x, data$y, 
                        control = list(ndeps = 1e-10))$coefficients, 
               tolerance = 0.0009)
  expect_equal(get_coefficients_from_glm(data), 
               logitreg(data$x, data$y, 
                        control = list(maxit = 50))$coefficients, 
               tolerance = 0.0009)
  
})

test_that("Wrong data types",{
  expect_warning(logitreg(as.data.frame(data$x), data$y))
  expect_error(logitreg(as.character(data$x), data$y))
  expect_error(logitreg(list(data$x), data$y))
  expect_error(logitreg(data$x, list(data$y)))
  expect_error(logitreg(data$x, sample(c(1,2), nrow(data$x), replace = TRUE)))
  expect_error(logitreg(data$x, sample(c(1,0), 150, replace = TRUE)))
})
          


