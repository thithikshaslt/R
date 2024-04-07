decision_matrix <- matrix(c(-20,60,30,-5,40,50,35,0,-50,100,45,-10,12,15,15,10), nrow=4,byrow=TRUE)

rownames(decision_matrix) <- c("Plant Corn","Plant Wheat","Plant Soyabeans","Grazing")
colnames(decision_matrix) <- c("Heavy Rainfall","Moderate Rainfall","Light Rainfall","Drought")

print("Hurwicz")

alpha <- 0.5

hur <- apply(decision_matrix,1,function(x) alpha*max(x) + (1-alpha)*min(x))
hur_max <- max(hur)
hur_opt <- which(hur == hur_max)
print(hur_opt)
