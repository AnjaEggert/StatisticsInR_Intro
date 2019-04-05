setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_2019")

Compute the matrix of all correlations between the measurements from the turtles data.

cor(turtles[,3:4])

turtles data
Produce all pairwise scatterplots,
as well as the one-dimensional histograms on the diagonal
Use the package "GGally"

library(GGally)
ggpairs(turtles[, -1], axisLabels = "none")

##----Turtles----
turtles = read.table("./data/PaintedTurtles.txt", header = TRUE)
# Show the first 4 rows of the data set

# Make a heatmap of the correlations of the athletes data.
# Use the package "pheatmap".
library(pheatmap)
pheatmap(cor(athletes))

##----Athletes----
load("./data/athletes.RData")
# Plot the performance for the 100 m with ggplot
athletes$person <-seq(1:33)
library(ggplot2)

ggplot(data=athletes, aes(x=person,col=category)) +
  geom_line(aes(y=m100*5,colour="orange",size=2))+
  geom_point(aes(y=m400,colour="blue"))+
  scale_color_discrete(name = "Athletes", labels = c("m100", "m400"))

ggplot()+
  geom_line(data=Summary,aes(y=Y1,x= X,colour="darkblue"),size=1 )+
  geom_line(data=Summary,aes(y=Y2,x= X,colour="red"),size=1) +
  scale_color_discrete(name = "Y series", labels = c("Y2", "Y1"))

##----gene expression ----
load("./data/Msig3transp.RData")
# Round the numbers to 2 digits
Msig3transp = round(Msig3transp,2)

##----Bacterial Species Abundances----
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq", version = "3.8")

data("GlobalPatterns", package = "phyloseq")
# Extract the out_table as a matrix

##----mass spectroscopy peaks----
metab = t(as.matrix(read.csv("./data/metabolites.csv", row.names = 1)))
