library(numDeriv)

# Define the function
f <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  return(x1 - x2 + 2*x1^2 + 2*x1*x2 + x2^2)
}

# Function to compute the gradient using numerical differentiation
compute_gradient <- function(f, x) {
  gradient <- grad(func = f, x = x)
  return(gradient)
}

# Gradient of the function using numerical differentiation
grad_f <- function(x) {
  gradient <- compute_gradient(f, x)
  return(gradient)
}


# Cauchy Method
cauchy_method <- function(f, grad_f, x0) {
  x <- x0
  for(i in 1:50) {
    # Compute the gradient at the current point
    grad <- grad_f(x)
    grad <- round(grad,4)
    # Check convergence
    if (all(grad == 0)) {
      break
    }
    
    # Compute the Cauchy point
    s <- -grad_f(x)
    
    # Line search
    line_search <- function(lambda) f(x + lambda * s)
    opt <- optimize(f = line_search, interval = c(0, 10), maximum = FALSE)
    lambda <- opt$minimum
    x <- x + lambda * s
  }
  return(list(x = x, value = f(x)))
}

# Initial point
x0 <- c(0, 0)

# Run Cauchy method
result <- cauchy_method(f, grad_f, x0)
cat("\nMinimum point:", round(result$x,2), "\n")
cat("Minimum value:", round(result$value,2), "\n")
