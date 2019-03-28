setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_2019")

##----Turtles----
turtles = read.table("./data/PaintedTurtles.txt", header = TRUE)
# Show the first 4 rows of the data set


##----Athletes----
load("./data/athletes.RData")
# Plot the performance for the 100 m with ggplot

##----gene expression ----
load("./data/Msig3transp.RData")
# Round the numbers to 2 digits

##----Bacterial Species Abundances----
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq", version = "3.8")

data("GlobalPatterns", package = "phyloseq")
# Extract the out_table as a matrix

##----mass spectroscopy peaks----
metab = t(as.matrix(read.csv("./data/metabolites.csv", row.names = 1)))
