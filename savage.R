decision_matrix <- matrix(c(-20,60,30,-5,40,50,35,0,-50,100,45,-10,12,15,15,10), nrow=4,byrow=TRUE)

rownames(decision_matrix) <- c("Plant Corn","Plant Wheat","Plant Soyabeans","Grazing")
colnames(decision_matrix) <- c("Heavy Rainfall","Moderate Rainfall","Light Rainfall","Drought")

print("savage regret")
savage <- apply(decision_matrix,2,function(x) max(x)-x)
print("regret table")
print(savage)
minmax <- apply(savage,1,max)
print(minmax)
min_val <- min(minmax)
min_opt <- which(minmax == min_val)

print(min_opt)
