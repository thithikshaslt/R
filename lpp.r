library("lpSolve")

obj_coeff <- c(5, 4)
cons_coeff <- matrix(c(6, 4, 1, 2, -1, 1, 0, 1), nrow = 4, byrow = TRUE)
cons_rhs <- c(24, 6, 1, 2)
cons_dir <- c("<=", "<=", "<=", "<=")

res <- lp("max", obj_coeff, cons_coeff, cons_dir, cons_rhs)

print(res$objval) #optimum value
print(res$solution) #points