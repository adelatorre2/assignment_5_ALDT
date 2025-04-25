#' @title Assignment 5: Statistical Estimation and Probability Distributions
#'
#' @description
#' This script solves all four problems from Assignment 5 in MT220-01: Introduction to Probability 
#' and Statistics. It covers confidence interval estimation for height and weight, properties of 
#' the normal and exponential distributions, and sample size calculations.
#'
#' @details
#' Part 1 computes 95% confidence intervals for height and weight using gender-separated samples 
#' from a dataset. Part 2 analyzes a normal distribution to evaluate its PDF, tail probabilities, 
#' and central moments. Part 3 constructs confidence intervals using known variance and derives 
#' the sample size required for a desired margin of error. Part 4 explores the exponential 
#' distribution’s PDF, mean, variance, and adapts it to match a specific variance target.
#'
#' @author Alejandro De La Torre
#' @date April 23, 2025
#'
#' @seealso
#' Course Textbook: Montgomery & Runger - Applied Statistics and Probability for Engineers
#'
#' @examples
#' # Run this script top-down in RStudio or VS Code to reproduce figures and calculations.
#'
#' @note
#' This script assumes the working directory is set to the assignment_5_ALDT folder 
#' and that all necessary subdirectories (e.g., report/figures) exist.

# ---------------------------------------------
# Part 1: Confidence Intervals for Weight & Height
# ---------------------------------------------

# Load required libraries
library(readxl)
library(dplyr)

# Load dataset
data <- read_excel("weight-height.xls")

# Preview dataset
str(data)

# Separate by gender
men <- data %>% filter(Gender == "Male")
women <- data %>% filter(Gender == "Female")

# Function to compute 95% confidence interval (known variance)
compute_ci <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  var_x <- var(x)
  sd_x <- sqrt(var_x)
  z <- 1.96  # 95% confidence
  error_margin <- z * sd_x / sqrt(n)
  lower <- mean_x - error_margin
  upper <- mean_x + error_margin
  return(c(mean = mean_x, lower = lower, upper = upper, n = n, sd = sd_x))
}

# Compute CIs for each variable and gender
ci_men_weight <- compute_ci(men$Weight)
ci_men_height <- compute_ci(men$Height)

ci_women_weight <- compute_ci(women$Weight)
ci_women_height <- compute_ci(women$Height)

# Display results
cat("95% CI for Men's Weight:", round(ci_men_weight[2:3], 2), "\n")
cat("95% CI for Men's Height:", round(ci_men_height[2:3], 2), "\n")
cat("95% CI for Women's Weight:", round(ci_women_weight[2:3], 2), "\n")
cat("95% CI for Women's Height:", round(ci_women_height[2:3], 2), "\n")

# Visualize distributions
library(ggplot2)

# Plot: Men's Weight
p_men_weight <- ggplot(men, aes(x = Weight)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Men's Weight", x = "Weight (kg)", y = "Frequency") +
  theme_minimal()
ggsave("report/figures/mens_weight_distribution.png", plot = p_men_weight, width = 8, height = 6)

# Plot: Men's Height
p_men_height <- ggplot(men, aes(x = Height)) +
  geom_histogram(binwidth = 2, fill = "forestgreen", color = "white") +
  labs(title = "Distribution of Men's Height", x = "Height (cm)", y = "Frequency") +
  theme_minimal()
ggsave("report/figures/mens_height_distribution.png", plot = p_men_height, width = 8, height = 6)

# Plot: Women's Weight
p_women_weight <- ggplot(women, aes(x = Weight)) +
  geom_histogram(binwidth = 2, fill = "tomato", color = "white") +
  labs(title = "Distribution of Women's Weight", x = "Weight (kg)", y = "Frequency") +
  theme_minimal()
ggsave("report/figures/womens_weight_distribution.png", plot = p_women_weight, width = 8, height = 6)


# Plot: Women's Height
p_women_height <- ggplot(women, aes(x = Height)) +
  geom_histogram(binwidth = 2, fill = "orchid", color = "white") +
  labs(title = "Distribution of Women's Height", x = "Height (cm)", y = "Frequency") +
  theme_minimal()
ggsave("report/figures/womens_height_distribution.png", plot = p_women_height, width = 8, height = 6)


# ---------------------------------------------
# Part 2: Normal Distribution Analysis
# ---------------------------------------------

# Parameters for the normal distribution
mu_x <- 5
sigma2_x <- 1
sigma_x <- sqrt(sigma2_x)

# a) PDF expression
pdf_normal <- function(x) {
  return((1 / (sigma_x * sqrt(2 * pi))) * exp(-0.5 * ((x - mu_x)^2 / sigma2_x)))
}

# Evaluate pdf at mean (for reference)
cat("PDF evaluated at X = 5:", pdf_normal(5), "\n")

# b) P(X > 6)
prob_x_gt_6 <- pnorm(6, mean = mu_x, sd = sigma_x, lower.tail = FALSE)
cat("P(X > 6):", round(prob_x_gt_6, 4), "\n")

# c) P(3 < X < 6)
prob_3_lt_x_lt_6 <- pnorm(6, mean = mu_x, sd = sigma_x) - pnorm(3, mean = mu_x, sd = sigma_x)
cat("P(3 < X < 6):", round(prob_3_lt_x_lt_6, 4), "\n")

# d) Expected value and variance
cat("Expected value E(X):", mu_x, "\n")
cat("Variance Var(X):", sigma2_x, "\n")



# ---------------------------------------------
# Part 3: Confidence Intervals with Known Variance
# ---------------------------------------------

# Given parameters
sigma2_known <- 4
sigma_known <- sqrt(sigma2_known)
x_bar <- 123
n_sample <- 1000

# a) 99% CI for the actual mean
z_99 <- qnorm(1 - 0.01 / 2)
margin_error_99 <- z_99 * sigma_known / sqrt(n_sample)
ci_99 <- c(lower = x_bar - margin_error_99, upper = x_bar + margin_error_99)
cat("99% CI for the mean:", round(ci_99, 4), "\n")

# b) 95% CI for the actual mean
z_95 <- qnorm(1 - 0.05 / 2)
margin_error_95 <- z_95 * sigma_known / sqrt(n_sample)
ci_95 <- c(lower = x_bar - margin_error_95, upper = x_bar + margin_error_95)
cat("95% CI for the mean:", round(ci_95, 4), "\n")

# c) Required sample size for margin of error E <= 0.1 at 95% confidence
E_target <- 0.1
required_n <- ceiling((z_95 * sigma_known / E_target)^2)
cat("Minimum required sample size for E <= 0.1:", required_n, "\n")


# d) Expected value and variance
cat("Expected value:", x_bar, "\n")
cat("Known variance:", sigma2_known, "\n")


# ---------------------------------------------
# Part 4: Exponential Distribution Analysis
# ---------------------------------------------

# Given parameter for the exponential distribution
lambda_exp <- 2

# a) PDF of the exponential distribution
pdf_exponential <- function(x, lambda) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

# Display PDF formula for lambda = 2
cat("PDF of exponential distribution with lambda = 2:\n")
cat("f(x) = 2 * exp(-2 * x) for x >= 0\n\n")

# b) Mean and variance for exponential distribution
mean_exp <- 1 / lambda_exp
var_exp <- 1 / (lambda_exp^2)
cat("Mean:", mean_exp, "\n")
cat("Variance:", var_exp, "\n\n")

# c) Find the lambda that gives a variance of 4
target_variance <- 4
lambda_new <- 1 / sqrt(target_variance)
cat("Lambda for variance = 4:", lambda_new, "\n")

# New PDF with the adjusted lambda
cat("PDF of exponential distribution with variance = 4:\n")
cat(paste0("f(x) = ", round(lambda_new, 3), " * exp(-", round(lambda_new, 3), " * x) for x >= 0\n"))


# OPTIONAL PDF PLOT
# Parameters for the plot
lambda <- 2  # Example lambda value
x_values <- seq(0, 5, by = 0.01)  # Range of x values
y_values <- pdf_exponential(x_values, lambda)  # Compute PDF values

# Create a data frame for plotting
pdf_data <- data.frame(x = x_values, y = y_values)

# Plot the PDF using ggplot2
library(ggplot2)
p_pdf <- ggplot(pdf_data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "PDF of Exponential Distribution",
       x = "x",
       y = "f(x)") +
  theme_minimal()

# Save the plot
ggsave("report/figures/exponential_pdf_plot.png", plot = p_pdf, width = 8, height = 6)

# Display the plot
print(p_pdf)