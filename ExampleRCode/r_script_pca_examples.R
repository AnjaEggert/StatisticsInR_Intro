setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_2019")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("xcms", version = "3.8")

library("xcms")
library("ade4")
library("factoextra")
library("tidyverse")
load("./data/mat1xcms.RData")
dim(mat1)

pcamat1 = dudi.pca(t(mat1), scannf = FALSE, nf = 3)
fviz_eig(pcamat1, geom = "bar", bar_width = 0.7) + ggtitle("")

dfmat1 = cbind(pcamat1$li, tibble(
  label = rownames(pcamat1$li),
  number = substr(label, 3, 4),
  type = factor(substr(label, 1, 2))))

pcsplot = ggplot(dfmat1,aes(x=Axis1, y=Axis2, label=label, group=number, colour=type)) +
  geom_text(size = 4, vjust = -0.5)+
  geom_point(size = 3)+ylim(c(-18,19))

pcsplot + geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2)

# do the samples seem to be randomly placed in the plane? Do you notice any structure explained by the labels?
pcsplot + geom_line(colour = "red")


library("pheatmap")
load("./data/wine.RData")
load("./data/wineClass.RData")

pheatmap(1 - cor(wine), treeheight_row = 0.2)

winePCAd = dudi.pca(wine, scannf=FALSE)
table(wine.class)

fviz_pca_biplot(winePCAd, geom = "point", habillage = wine.class,
                col.var = "violet", addEllipses = TRUE, ellipse.level = 0.69) +
  ggtitle("") +
  coord_fixed()