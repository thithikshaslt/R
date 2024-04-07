decision_matrix <- matrix(c(-20,60,30,-5,40,50,35,0,-50,100,45,-10,12,15,15,10), nrow=4,byrow=TRUE)

rownames(decision_matrix) <- c("Plant Corn","Plant Wheat","Plant Soyabeans","Grazing")
colnames(decision_matrix) <- c("Heavy Rainfall","Moderate Rainfall","Light Rainfall","Drought")

print(decision_matrix)

print("Laplace Method")
laplace <- rowMeans(decision_matrix)
print(laplace)
optimal <- max(laplace) #if payoff
opt_index <- which(laplace == optimal)

cat("Choose option", opt_index[1])
#print(optimal)
#print(opt_index)
