#installing and importing packages; ISLR is a test database: https://cran.r-project.org/web/packages/ISLR/ISLR.pdf, https://hastie.su.domains/ISLR2/ISLRv2_corrected_June_2023.pdf
install.packages(c('ggplot2', 'psych', 'tidyverse', 'ISLR2', 'dplyr'))
library(psych)
library(ggplot2)
library(tidyr)
library(ISLR2)
library(dplyr)

#Creating the Sequential Regression

#generating sample data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 2*x1 + 3*x2 + 4*x3 + rnorm(n)
data <- data.frame(x1, x2, x3, y)
describe(data)

#need to figure out a way to make this for n vars, tho data will only have 3-4
#lm() --> 'linear model'
model <- lm(y ~ ., data = data)
rSquare <- summary(model)$r.squared[-1]

cat("Proportion of Variance Explained by Each Variable:\n")
for (i in 1:length(rsquared)) {
  cat(paste(names(rsquared)[i], ": ", formatC(rsquared[i] * 100, digits = 2), "%\n"))
}


#anova(m1, m2, m3, m4)























#sources:
#https://educationalresearchtechniques.com/2019/02/11/hierarchical-regression-in-r/
#https://towardsdatascience.com/7-steps-to-run-a-linear-regression-analysis-using-r-d5897a66b835
