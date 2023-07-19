#installing and importing packages; ISLR is a test database: https://cran.r-project.org/web/packages/ISLR/ISLR.pdf, https://hastie.su.domains/ISLR2/ISLRv2_corrected_June_2023.pdf
install.packages(c('ggplot2', 'psych', 'tidyverse', 'ISLR2', 'dplyr'))
library(psych)
library(ggplot2)
library(tidyr)
library(ISLR2)
library(dplyr)


#algo: pull num vars from data set (store in either hashmap or array)
#      start with an empty model (no vars only intercept)
#      add first var, store model, R, R^2, and other stats
#      add 2nd var, store model, R, R2, and other stats, as well as current R^2 - prev R^2
#      add 3rd var, store model, R, R2, and other stats, as well as current R^2 - prev R^2
#      continue through n variables
#      return: R^2 for first variable, and R^2 - prev R^2 for each successive variable, to figure out the effect of each variable on the data set
#      Afterwards: time series data of each stock, price change from time of report release to \

#generate data w 3 variables
datagen <- function()
  {
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  y <- 2*x1 + 3*x2 + 4*x3 + rnorm(n)
  data <- data.frame(x1, x2, x3, y)
  return(data)
  }


# Function to calculate adjusted R-squared
adjusted_r_squared <- function(model, data) {
  n <- length(model$residuals)
  p <- model$rank - 1
  r_squared <- summary(model)$r.squared
  adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
  return(adjusted_r_squared)
}

# Function to perform variable selection and calculate R-squared differences
variable_selection <- function(dataset, num_vars) {
  selected_vars <- vector(mode = "list", length = num_vars)
  r_squared_diff <- vector(mode = "numeric", length = num_vars - 1)
  
  for (i in 1:num_vars) {
    if (i == 1) 
    {
      model <- lm(y ~ 1, data = dataset)
    } 
    else 
    {
      model <- lm(y ~ ., data = dataset[, selected_vars[[i-1]]])
    }
    
    selected_vars[[i]] <- names(coefficients(model))
    
    # Calculate R-squared and adjusted R-squared
    r_squared <- summary(model)$r.squared
    adjusted_r_squared <- adjusted_r_squared(model, dataset)
    
    # Store R-squared difference
    if (i > 1) 
    {
      r_squared_diff[i-1] <- r_squared - r_squared_prev
    }
    
    cat("\nModel", i, "Summary:\n")
    print(summary(model))
    
    cat("R-squared:", r_squared, "\n")
    cat("Adjusted R-squared:", adjusted_r_squared, "\n")
    
    r_squared_prev <- r_squared
  }
  
  # Return R-squared for the first variable and R-squared differences
  return(list(r_squared_first = summary(lm(y ~ selected_vars[[1]], data = dataset))$r.squared, 
              r_squared_diff = r_squared_diff))
}

# Runtime
#dataset <- read.csv("your_dataset.csv")
dataset <- data
num_vars <- 3

results <- variable_selection(dataset, num_vars)

# Print R-squared for the first variable
cat("\nR-squared for the first variable:", results$r_squared_first, "\n")

# Print R-squared differences
cat("R-squared differences:\n")
print(results$r_squared_diff)

#Creating Chi-Square Tests of significance (for different periods of time: 1D, 5D, 1M, 6M, 1Y) for each indicator
#read csv of stock prices, read data set of when reports are released (for now, generate price data)
#store price history in a b-tree
#find differences and if > or < 0 store true or false into arraylist (for each time period) & find num of true and falses (also for each time period)
#run individual chi square tests on num of trues and falses to find significance level (ie is there significant evidence that x indicator predicts increase in price over y time period)
#return results of tests
