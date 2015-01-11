url <- "http://www.statistik.lmu.de/~scheipl/downloads/fortprog/"
source(paste0(url, "simulate_logitreg.R"))
data <- simulate_logitreg(seed = 555, q_numeric = 1, q_factor = 1, n = 100,
                          data.frame = TRUE)
model_logitreg <- logitreg(y ~ . , data = data)
model_glm <- glm(y ~ ., data = data, family = binomial)

expect_equal(predict(model_logitreg), 
             predict(model_glm, type = "response"),
             tolerance = 0.0009)

new_data <- data.frame(X1 = rnorm(100), 
                       X2 = sample(c(1,0), 100, replace = TRUE))

expect_equal(predict(model_logitreg, new_data), 
             predict(model_glm, newdata = new_data, type = "response"),
             tolerance = 0.0009)

expect_equal(fitted(model_logitreg), fitted(model_glm), tolerance = 0.0009)

#test for ROC plot omitted, no way to check a plot

