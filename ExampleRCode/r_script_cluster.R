##----Install ggbio package from Bioconductor----

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggbio", version = "3.8")

##----Ideogram----
library("ggbio")
data("hg19IdeogramCyto", package = "biovizBase")

plotIdeogram(hg19IdeogramCyto, subchr = "chr1")

##----Ideogram----
library("GenomicRanges")
data("darned_hg19_subset500", package = "biovizBase")


##----Reorder chromosome lengths----
autoplot(darned_hg19_subset500, layout = "karyogram",
         aes(color = exReg, fill = exReg))

data("ideoCyto", package = "biovizBase")
dn = darned_hg19_subset500
seqlengths(dn) = seqlengths(ideoCyto$hg19)[names(seqlengths(dn))]
dn = keepSeqlevels(dn, paste0("chr", c(1:22, "X")))
autoplot(dn, layout = "karyogram", aes(color = exReg, fill = exReg))

##----Buidling circular plot ----
data("CRC", package = "biovizBase")

autoplot(hg19sub, layout = "circle", fill = "gray70")

ggbio() +
  circle(hg19sub, geom = "ideo", fill = "gray70") +
  circle(hg19sub, geom = "scale", size = 2) +
  circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3)

ggbio(trackWidth = 10, buffer = 0, radius = 10) +
  circle(hg19sub, geom = "ideo", fill = "gray70") +
  circle(hg19sub, geom = "scale", size = 2) +
  circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3)


