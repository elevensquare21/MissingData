#make boxplots to compare model performances for each of the covariates: age, bmi, depression, and weekend

load("/Users/Selene/Desktop/Updated Missing Data/results/m0 result.rdata")
load("/Users/Selene/Desktop/EM impute/modelimp result.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/adjust for wt/m00.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/m1.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/m4.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/rm1.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/rm2.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/results/knn.result.rdata")


m0=m0[,c(4,7,10,13)]
m00=m00[,c(4,7,10,13)]
m.imp=m.imp[,c(4,7,10,13)]
m1=m1[,c(4,7,10,13)]
m4=m4[,c(4,7,10,13)]
rm1=rm1[,c(4,7,10,13)]
rm2=rm2[,c(4,7,10,13)]
knn=knn.result[,c(4,7,10,13)]

age=c(m0[,1],m00[,1],m.imp[,1],m1[,1],m4[,1],rm1[,1],rm2[,1],knn[,2])
models=c(rep('a',100),rep('b',100),rep('c',100),rep('d',100),rep('e',100),rep('f',100),rep('g',100),rep('h',100))
age=data.frame(age,models)
#save(age,file='age.rdata')
library('ggplot2')
title="Compare Performances of Different Methods in Estimating the Age Coefficient"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="none", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(age, mapping=aes_string(y = "age", x = "models")) + geom_boxplot(outlier.colour = NULL, aes_string(colour="models", fill="models")) +  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })+geom_abline(intercept=-4.6003,slope=0)


bmi=c(m0[,2],m00[,2],m.imp[,2],m1[,2],m4[,2],rm1[,2],rm2[,2],knn[,2])
models=c(rep('a',100),rep('b',100),rep('c',100),rep('d',100),rep('e',100),rep('f',100),rep('g',100),rep('h',100))
bmi=data.frame(bmi,models)
#save(bmi,file='bmi.rdata')
library('ggplot2')
title="Compare Performances of Different Methods in Estimating the BMI Coefficient"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="none", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(bmi, mapping=aes_string(y = "bmi", x = "models")) + geom_boxplot(outlier.colour = NULL, aes_string(colour="models", fill="models")) +  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })+geom_abline(intercept=-4.1768,slope=0)

depression=c(m0[,3],m00[,3],m.imp[,3],m1[,3],m4[,3],rm1[,3],rm2[,3],knn[,3])
models=c(rep('a',100),rep('b',100),rep('c',100),rep('d',100),rep('e',100),rep('f',100),rep('g',100),rep('h',100))
depression=data.frame(depression,models)
#save(depression,file='depression.rdata')
library('ggplot2')
title="Compare Performances of Different Methods in Estimating the Depression Indicator Coefficient"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="none", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(depression, mapping=aes_string(y = "depression", x = "models")) + geom_boxplot(outlier.colour = NULL, aes_string(colour="models", fill="models")) +  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })+geom_abline(intercept=-30.9295,slope=0)


weekend=c(m0[,4],m00[,4],m.imp[,4],m1[,4],m4[,4],rm1[,4],rm2[,4],knn[,4])
models=c(rep('a',100),rep('b',100),rep('c',100),rep('d',100),rep('e',100),rep('f',100),rep('g',100),rep('h',100))
wnd=data.frame(weekend,models)
#save(wnd,file='wnd.rdata')
library('ggplot2')
title="Compare Performances of Different Methods in Estimating the Weekend Indicator Coefficient"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="none", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(wnd, mapping=aes_string(y = "weekend", x = "models")) + geom_boxplot(outlier.colour = NULL, aes_string(colour="models", fill="models")) +  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })+geom_abline(intercept=8.3622,slope=0)





