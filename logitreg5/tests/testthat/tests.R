library(testthat)
url <- "http://www.statistik.lmu.de/~scheipl/downloads/fortprog/"
source(paste0(url, "simulate_logitreg.R"))
library(logitreg5)
data <- simulate_logitreg(seed = 555, q_numeric = 1, q_factor = 1, n = 100)
data_df <- simulate_logitreg(seed = 555, q_numeric = 1, q_factor = 1, n = 100,
                             data.frame = TRUE)
m_0 <- logitreg(design = data$x, response = data$y)
m_1 <- logitreg(design = y ~ X1 + X2, data = data_df)
m_2 <- logitreg(y ~ ., data_df)
y <- factor(data_df$y + 1)
X1 <- data_df$X1
X2 <- factor(data_df$X2)
model_formula <- y ~ X1 + X2
m_3 <- logitreg(design = model_formula)
m_4 <- logitreg(design = list(rep(1, 100), data$x[, -1]), response = data$y)
# should all pass without error:
expect_equivalent(m_0, m_1)
expect_equivalent(m_0, m_2)
expect_equivalent(m_0, m_3)
expect_equivalent(m_0, m_4)
# should yield TRUE:
expect_true(all(sapply(list(m_0, m_1, m_2, m_3, m_4), class) == "logitreg"))