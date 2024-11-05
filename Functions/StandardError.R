# Function to calculate standard error
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}