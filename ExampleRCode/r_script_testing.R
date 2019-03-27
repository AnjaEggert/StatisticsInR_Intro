rm(list=ls())
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_2019/testing")

## ----whatprob1-----------------------------------------------------------
set.seed(0xdada)

numFlips = 100
probHead = 0.6
coinFlips = sample(c("H", "T"), size = numFlips,
  replace = TRUE, prob = c(probHead, 1 - probHead))
head(coinFlips)

## ----tableCoinFlips------------------------------------------------------
table(coinFlips)

## ----binomDens-----------------------------------------------------------
library("dplyr")
k = 0:numFlips
numHeads = sum(coinFlips == "H")
binomDensity = tibble(k = k,
     p = dbinom(k, size = numFlips, prob = 0.5))

## ----dbinom distribution----
library("ggplot2")
ggplot(binomDensity) +
  geom_bar(aes(x = k, y = p), stat = "identity") +
  geom_vline(xintercept = numHeads, col = "blue")

## ----rejection region----
library("dplyr")
alpha = 0.05
binomDensity = arrange(binomDensity, p) %>%
        mutate(reject = (cumsum(p) <= alpha))

ggplot(binomDensity) +
  geom_bar(aes(x = k, y = p, col = reject), stat = "identity") +
  scale_colour_manual(
    values = c(`TRUE` = "red", `FALSE` = "darkgrey")) +
  geom_vline(xintercept = numHeads, col = "blue") +
  theme(legend.position = "none")


## ----binom.test----------------------------------------------------------
binom.test(x = numHeads, n = numFlips, p = 0.5)
