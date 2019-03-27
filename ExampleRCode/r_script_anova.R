setwd("~/FBN/statistics_teaching/graduiertenakademie_hro_2019")

##---read excel data-----

library("readxl")

dat<-read_xlsx("urchins.xlsx")

##---long versus wide format-----
library("tidyverse")

dat_long <- gather(dat)

##---ANOVA-----
mod <- lm(value ~ key, data=dat_long)

anova(mod)

##---multiple comparison of means-----
library("emmeans")

means <- emmeans(mod,pairwise ~key, adjust ="tukey")

means$emmeans

means$contrasts

means <- CLD(means$emmeans, Letters=letters)
means


##---plots-----

library(ggplot2)

plot1<-ggplot(data=means, aes(x=key))+
  geom_bar(aes(y=emmean),stat="identity",width=0.8)+
  geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.4)+
  geom_text(aes(y=(emmean+SE)*1.1,label=.group))+
  labs(y="Adjusted means +/- standard error",x="key",caption="Tykey test")


plot2<-ggplot()+
  theme_classic()+
  geom_boxplot(data=dat_long,aes(x=key, y=value),outlier.shape=NA, width=0.6)+
  geom_jitter(data=dat_long,aes(x=key,y=value),width=0.25,height=0,shape=1)+
  geom_point(data=means,aes(x=as.numeric(key)+0.4,y=emmean),col="cornflowerblue",shape=16,size=2)+
  geom_errorbar(data=means,aes(x=as.numeric(key)+0.4,ymin=lower.CL,ymax=upper.CL),col="cornflowerblue",width=0.4)+
  geom_text(data=means,aes(x=as.numeric(key)+0.5,y=emmean,label=.group),col="firebrick")



png("anova_urchins.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
plot2
dev.off()



