#' @title assignment_5.R
#'
#' @description
#' [Provide a brief description of what the script does. Include the purpose
#' and any relevant context or background information.]
#'
#' @details
#' [Provide additional details about the script, such as assumptions, 
#' limitations, or any specific behavior that users should be aware of.]
#'
#' @author [Your Name or Team Name]
#' @date [Date of creation or last modification]
#'
#' @usage
#' [Provide an example of how to use the script, if applicable.]
#'
#' @seealso
#' [List any related scripts, functions, or documentation that may be helpful.]
#'
#' @examples
#' [Provide example usage or expected output, if relevant.]
#'
#' @note
#' [Include any additional notes or warnings, if necessary.]

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
