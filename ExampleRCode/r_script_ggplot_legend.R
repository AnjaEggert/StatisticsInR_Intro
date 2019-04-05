rm(list=ls())
setwd("D:/Eggert/Documents/GitHub/StatisticsInR_Intro/ExampleRCode")

load("../ExampleData/data_wide.RData")

# the loaded data set is in the wide format, i.e. each variable is written in one column
# to use legend in ggplot, the best approach is to convert the data to long format
# using melt() from package reshape or reshape2
# or using gather() from the tidyr package:

##----Convert data frame to long format----
library("tidyr")
data_wide <- as_tibble(data_wide)

# convert to long format
data_long <- gather(data_wide, variable, value,var0:var1)

head(data_wide,3)
# # A tibble: 3 x 3
#   var0  var1 date      
#   <dbl> <dbl> <date>    
# 1 100    150  2002-01-01
# 2  92.1  146. 2002-02-01
# 3  80.7  143. 2002-03-01

head(data_long,3)
# # A tibble: 3 x 3
#   date       variable value
#   <date>     <chr>    <dbl>
# 1 2002-01-01 var0     100  
# 2 2002-02-01 var0      92.1
# 3 2002-03-01 var0      80.7

##----Plotting----
library("ggplot2")
ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line()

ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=variable))+
  scale_color_manual(values=c('cornflowerblue','orange'))

ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=variable), size=2)+
  scale_color_manual(values=c('cornflowerblue','orange'))+
  theme(legend.position="top")

# Define colour palette outside ggplot
cbPalette <- c("#009E73", "#F0E442")

ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=variable, linetype=variable), size=1.5)+
  scale_color_manual(values=cbPalette)+
  # here the order of the lines is important!
  # as them_bw() defines legend position at the right side, but if you want it on top, the poistion line must follow the theme line
  theme_bw()+
  theme(legend.position="top")

ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=variable, linetype=variable), size=1.5)+
  scale_color_manual(values=cbPalette)+
  theme_bw()+
  theme(legend.position=c(0.1, 0.1))  # define specific position with relativ values for x and y, (0,0) is in the lower left corner

ggplot(data=data_long,aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=variable,linetype=variable), size=1.5)+
  scale_color_manual(values=cbPalette,labels=c("Variable 0", "Variable 1"))+
  labs(x="Date", y="Growth", col="Variable \neffects")+
  scale_linetype(guide = FALSE)+   # you need to exclude the legend for line type, otherwise you would get 2 legends
  theme_bw()

# use %>% with ggplot
cbPalette <- c("#009E73", "#F0E442")

data_long %>% 
  ggplot(aes(x=date, y=value, group=variable)) +
    geom_line(aes(colour=variable,linetype=variable), size=1.5)+
    scale_color_manual(values=cbPalette,labels=c("Variable 0", "Variable 1"))+
    labs(x="Date", y="Growth", col="Variable \neffects")+
    scale_linetype(guide = FALSE)+   # you need to exclude the legend for line type, otherwise you would get 2 legends
    theme_bw()
