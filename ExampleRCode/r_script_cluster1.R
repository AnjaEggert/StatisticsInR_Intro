setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_2019")

load("./data/Msig3transp.RData")
save.image("cluster.RData")


# Round the numbers to 2 digits
Msig3transp = round(Msig3transp,2)


# Ward Hierarchical Clustering
d <- dist(Msig3transp, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 
