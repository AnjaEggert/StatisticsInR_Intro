rm(list=ls())
setwd("C:/Users/eggert/ownCloud/fbn_PhdCourse_2018_ExpDesign/analyses/")

dat <- read.delim("data/1f_crd_mice.txt")

str(dat)
# 'data.frame':	9 obs. of  3 variables:
# $ line     : Factor w/ 3 levels "ctr","fert_A",..: 1 1 1 2 2 2 3 3 3
# $ replicate: int  1 2 3 1 2 3 1 2 3
# $ mass     : num  198 206 202 215 220 ...

# Completely randomized design (CRD)
# 1 fixed factor = mouse line
# 3 replicates each
# measured trait = body mass
# one-Way ANOVA = glm

dat$line   <- as.factor(dat$line)
dat$replicate <- as.factor(dat$replicate)

# plot for first impression
plot(y=dat$mass, x=dat$line)

# Fit general linear model 
###########################
# Treatment effects: mouse line
# Design effects:    -
# Step 1: Check F-Test of ANOVA
# Step 2: Compare adjusted means per level
mod <- lm(data    = dat,
          formula = mass ~ line)

library(ggfortify)
autoplot(mod) # Residual plots
mod           # Basic results
summary(mod)  # More detailed results
anova(mod)    # ANOVA-table: Variety effect is significant

library(emmeans) # also needs package multcompView to be installed

# get means and comparisons
means  <- emmeans(mod, pairwise ~ line, adjust = "tukey") # to get t-test: adjust="none"
means # look at means and differences between means
means$emmeans   # look at means
means$contrasts # look at differences between means

# add letters indicating significant differences between means
output <- CLD(means$emmeans, Letters=letters)

# plot adjusted means - option 1

#install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data=output, aes(x=line))
p <- p + geom_bar(aes(y=emmean), stat="identity", width=0.8)
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.4)
p <- p + geom_text(aes(y=emmean+15, label=.group))

p # show plot

# plot adjusted means - option 2
output$.group <- gsub(" ", "", output$.group, fixed = TRUE) # remove spaces

q <- ggplot() + theme_classic()
  # Rohdaten (dat)
q <- q + geom_boxplot(data=dat, aes(x=line, y=mass), 
               outlier.shape=NA, width=0.6)
q <- q + geom_jitter(data=dat, aes(x=line, y=mass), 
              width=0.25, height=0, shape=1)
  # Ergebnisse (output)
q <- q + geom_point(data=output, aes(x=as.numeric(line)+0.4, y=emmean),
             col="red", shape=16, size=2)
q <- q + geom_errorbar(data=output, aes(x=as.numeric(line)+0.4, ymin=lower.CL, ymax=upper.CL),
                col="red", width=0.1)
q <- q + geom_text(data=output, aes(x=as.numeric(line)+0.5, y=emmean, label =.group),
                   col="red")

q # show plot  
  
  
  
  
  