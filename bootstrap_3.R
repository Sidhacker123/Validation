# Lab: The Bootstrap

## The Bootstrap
library(ISLR2)
library(boot)

lsat_scores <- c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
gpa_scores <- c(3.93, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)

# Calculating the correlation between LSAT scores and GPA
lsat_gpa_correlation <- cor(lsat_scores, gpa_scores)
print(lsat_gpa_correlation)


###
alpha.fn <- function(data, indices) 
{
  # Access data using the indices provided by boot
  X <- data[indices, 1]  
  Y <- data[indices, 2]  
  
  # Calculate the statistic
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

# Combine the data into a data frame for bootstrapping
data <- data.frame(lsat = lsat_scores, gpa = gpa_scores)
###
###
set.seed(123)  
results <- boot(data, alpha.fn, R = 1000)  

results

