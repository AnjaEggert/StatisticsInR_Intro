rm(list=ls())
setwd("D:/Eggert/Documents/FBN/statistics/R/ProbabilityDistributions/")

####
library(ggplot2)
library(MASS) #fitdistr()

# parameters groups 1 to 4
mu <- c(10, 20, 30, 40)
mu <- c(10, 15, 20, 25)
mu <- c(10, 12, 14, 16)

sigma <- 1
alpha<-0.05

# draw normal distribution 
#set.seed(42)

# draw normal distributions with sample size=10
y.1 <- rnorm(1000,mean=mu[1],sd=sigma)
y.2 <- rnorm(1000,mean=mu[2],sd=sigma)
y.3 <- rnorm(1000,mean=mu[3],sd=sigma)
y.4 <- rnorm(1000,mean=mu[4],sd=sigma)
y.0 <-c(y.1,y.2,y.3,y.4)

y.1.fit <- fitdistr(y.1, "normal")
y.2.fit <- fitdistr(y.2, "normal")
y.3.fit <- fitdistr(y.3, "normal")
y.4.fit <- fitdistr(y.4, "normal")
y.0.fit <- fitdistr(y.0, "normal")

y.1.fit$estimate
#     mean        sd 
#9.9742110 0.9877728  

x <- seq(min(y.1)-2,max(y.4)+2,length.out=1000)

y.11 <-dnorm(x,mean=y.1.fit$estimate[1],sd=y.1.fit$estimate[2])
y.21 <-dnorm(x,mean=y.2.fit$estimate[1],sd=y.2.fit$estimate[2])
y.31 <-dnorm(x,mean=y.3.fit$estimate[1],sd=y.3.fit$estimate[2])
y.41 <-dnorm(x,mean=y.4.fit$estimate[1],sd=y.4.fit$estimate[2])
y.01 <-dnorm(x,mean=y.0.fit$estimate[1],sd=y.0.fit$estimate[2])

df<-data.frame(x,y.11,y.21,y.31,y.41,y.01)

# figure
norm.ggplot <- ggplot()+
  geom_histogram(data=data.frame(y.1),aes(x=y.1,y=..density..), binwidth=0.5, color="black", fill="cornflowerblue",alpha=0.4)+
  geom_histogram(data=data.frame(y.2),aes(x=y.2,y=..density..), binwidth=0.5, color="black", fill="cornflowerblue",alpha=0.4)+
  geom_histogram(data=data.frame(y.3),aes(x=y.3,y=..density..), binwidth=0.5, color="black", fill="cornflowerblue",alpha=0.4)+
  geom_histogram(data=data.frame(y.4),aes(x=y.4,y=..density..), binwidth=0.5, color="black", fill="cornflowerblue",alpha=0.4)+
  geom_line(data=df,aes(x=x,y=y.11),col="blue",size=1)+
  geom_line(data=df,aes(x=x,y=y.21),col="blue",size=1)+
  geom_line(data=df,aes(x=x,y=y.31),col="blue",size=1)+
  geom_line(data=df,aes(x=x,y=y.41),col="blue",size=1)+
  geom_line(data=df,aes(x=x,y=y.01),col="firebrick",size=2)+
  scale_x_continuous(limits=c(min(df$x),max(df$x)))+
  scale_y_continuous()+
  xlab(label= c(expression(paste("Observations"))))+
  ylab(label= c(expression(paste("Relative frequency"))))+
  theme_bw() + # remove grey background
  theme(legend.position="none")+
  theme(axis.title.x = element_text(face="bold", size=14), axis.text.x  = element_text(size=12,angle = 0, vjust = 0.5)) +
  #theme(axis.title.x = element_blank(), axis.text.x=element_blank()) +
  theme(axis.title.y = element_text(face="bold", size=14), axis.text.y  = element_text(size=12))

norm.ggplot

png("./FIG/norm_4groups_smalldiff.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
norm.ggplot
dev.off()

png("./FIG/norm_4groups_largediff.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
norm.ggplot
dev.off()
