rm(list=ls())
setwd("D:/Eggert/Documents/GitHub/StatisticsInR_Intro/ExampleRCode")

library(data.table)
dat <- fread("../ExampleData/2f_rcbd_cow.txt")

##----2way ANOVA----
# Randomized Complete Block Design
# 2 fixed factors: nutrition and familiy
# in three barns = blocks = repetition
# Two-Way ANOVA with two fixed factors

##----Conversion of data types etc.----
# use str() to look at the data format in the data table
# str(dat)
# Classes 'data.table' and 'data.frame':	72 obs. of  6 variables:
# $ row  : int  2 3 2 1 2 3 4 4 3 1 ...
# $ col  : int  6 4 3 1 1 1 5 1 2 2 ...
# $ block: chr  "block1" "block1" "block1" "block1" ...
# $ nut  : chr  "N1" "N2" "N4" "N6" ...
# $ fam  : chr  "A" "A" "A" "A" ...
# $ yield: int  4520 5598 6192 8542 5806 7470 4034 6682 6869 6318 ...
# - attr(*, ".internal.selfref")=<externalptr> 

# to define the factors as factors in the model equation, they need to be factors
# block, nut and fam are given as characters "chr"
# as.factor() is the function to convert the data type
dat$nut<-as.factor(dat$nut)
dat$fam<-as.factor(dat$fam)
dat$block<-as.factor(dat$block)

# combine the columns "fam" and "nut" 
# useful to define the single combinations of the two factors in one column
dat$comb<-as.factor(paste0(dat$fam,"-",dat$nut))

# the columns "col" and "row" are the coordinates in the experimental design
# desplot() uses it to draw the design
dat$col<-as.factor(dat$col)
dat$row<-as.factor(dat$row)

##----Draw the experimental design----
library(desplot)

desplot(data=dat, form= block ~ col+row,
        text=nut, cex=1, col=fam,
        main="",
        out1=row, out1.gpar=list(col="grey50", lwd=1, lty=1),
        out2=col, out2.gpar=list(col="grey50", lwd=1, lty=1))

# plot for first impression
plot(y=dat$yield, x=dat$fam)
plot(y=dat$yield, x=dat$nut)
plot(y=dat$yield, x=dat$block)
boxplot(data=dat, yield ~ fam + nut, las=2)

##----Fit linear model----
# Treatment effects: fam, nut and their interaction
# Design effects:    block
# Step 1: Check F-Test of ANOVA and perform backwards elimination
# especially with more factors, this should be done to only include significant effects in the final model equation
# Step 2: Compare adjusted means per level

mod <- lm(data    = dat,
          formula = yield ~ nut + fam + nut:fam + block)

anova(mod) # Interaction is significant -> Final model

library(ggfortify)
autoplot(mod) # Residual plots

library(emmeans)
# get means and comparisons
means  <- emmeans(mod, pairwise ~ nut|fam, adjust = "tukey") # to get t-test: adjust="none"
means # look at means and comparisons

##----Simple plotting----
# add letters indicating significant differences
means  <- CLD(means$emmeans, details=T, Letters = letters)
plotit <- as.data.table(means$emmeans)

# plot adjusted means
library(ggplot2)
ggplot(data=plotit, aes(x=nut)) +
  geom_bar(aes(y=emmean, fill=nut), stat="identity", width=0.8) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.4) +
  geom_text(aes(y=emmean+1500, label=.group)) +
  facet_wrap(~fam) + 
  theme_bw()

##----Alternative plotting----
# remove spaces
plotit$.group <- gsub(" ", "", plotit$.group, fixed = TRUE) 

ggplot() + theme_bw() +
  # Rohdaten (crd)
  geom_boxplot(data=dat, 
               aes(x=nut, y=yield), 
               outlier.shape=NA, width=0.6) +
  geom_jitter(data=dat, 
              aes(x=nut, y=yield), 
              width=0.25, height=0, shape=1) +
  # Ergebnisse (means)
  geom_point(data=plotit, 
             aes(x=as.numeric(nut)+0.4, y=emmean), 
             col="red", shape=16, size=2) +
  geom_errorbar(data=plotit, 
                aes(x=as.numeric(nut)+0.4, ymin=lower.CL, ymax=upper.CL), 
                col="red", width=0.1) +  
  geom_text(data=plotit, 
            aes(x=nut, y=9600, label=.group), 
            col="red") +
  facet_wrap(~fam) + 
  ylim(0, 10000)
