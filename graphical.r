A <- matrix(c(6, 4, 1, 2, -1, 1, 0, 1), nrow = 4, byrow = TRUE)
b <- c(24, 6, 1, 2)
obj <- c(5, 4)

## LP model
# The polytope with the corner points
plotPolytope(
  A,
  b,
  obj,
  type = rep("c", ncol(A)),
  crit = "max",
  faces = rep("c", ncol(A)),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = FALSE,
  labels = NULL,
  argsFaces = list(argsGeom_polygon = list(fill = "red"))
)