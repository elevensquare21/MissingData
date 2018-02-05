load("/Users/Selene/Desktop/Updated Missing Data/extended data with activity and date.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/MVPA/aggregate data.rdata")
agg.m=agg
load("/Users/Selene/Desktop/Updated Missing Data/Sedentary/aggregate data.rdata")
agg.s=agg
load("/Users/Selene/Desktop/Updated Missing Data/days with more than 50% time.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 50%-60% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 60%-70% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 70%-80% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 80%-90% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 90%-100% time missing.rdata")
library(nlme)

agg$wt=1440-agg$miss
agg$sed.adj=agg$sed/agg$wt*720
agg$mvpa=agg.m$mvpa
agg$mvpa.adj=agg$mvpa/agg$wt*720
ag=na.omit(agg)
ag$prop=ag$miss/1440


#run the pairwise comparison algorithm to simulate missing patterns from "complete" profiles
sub=ag[ag$prop<0.5,]
sub5=ag[ag$prop>=0.5 & ag$prop<0.6,]
sub6=ag[ag$prop>=0.6 & ag$prop<0.7,]
sub7=ag[ag$prop>=0.7 & ag$prop<0.8,]
sub8=ag[ag$prop>=0.8 & ag$prop<0.9,]
sub9=ag[ag$prop>=0.9 & ag$prop<1,]
tots=list(sub5,sub6,sub7,sub8,sub9)
sum=nrow(ag)
perc=1:5
for (i in perc){
	perc[i]=nrow(tots[[i]])/sum
}
n=round(perc*nrow(sub))

simd=newd
ind5=1:nrow(sub)
sam5=sample(ind5,n[1])
samp5=sub[sam5,]
index5=1:nrow(sub5)
sample5=sub5[sample(index5,n[1]),]
for (i in 1:n[1]){
	simd[simd$dt==samp5[i,1] & simd$identifier==samp5[i,2],5]=newd5[newd5$dt==sample5[i,1] & 			newd5$identifier==sample5[i,2],5]
}
sub.a5=sub[-sam5,]
ind6=1:nrow(sub.a5)
sam6=sample(ind6,n[2])
samp6=sub.a5[sam6,]
index6=1:nrow(sub6)
sample6=sub6[sample(index6,n[2]),]
for (i in 1:n[2]){
	simd[simd$dt==samp6[i,1] & simd$identifier==samp6[i,2],5]=newd6[newd6$dt==sample6[i,1] & 			newd6$identifier==sample6[i,2],5]
}
sub.a6=sub.a5[-sam6,]
ind7=1:nrow(sub.a6)
sam7=sample(ind7,n[3])
samp7=sub.a6[sam7,]
index7=1:nrow(sub7)
sample7=sub7[sample(index7,n[3]),]
for (i in 1:n[3]){
	simd[simd$dt==samp7[i,1] & simd$identifier==samp7[i,2],5]=newd7[newd7$dt==sample7[i,1] & 			newd7$identifier==sample7[i,2],5]
}
sub.a7=sub.a6[-sam7,]
ind8=1:nrow(sub.a7)
sam8=sample(ind8,n[4])
samp8=sub.a7[sam8,]
index8=1:nrow(sub8)
sample8=sub8[sample(index8,n[4]),]
for (i in 1:n[4]){
	simd[simd$dt==samp8[i,1] & simd$identifier==samp8[i,2],5]=newd8[newd8$dt==sample8[i,1] & 			newd8$identifier==sample8[i,2],5]
}
sub.a8=sub.a7[-sam8,]
ind9=1:nrow(sub.a8)
sam9=sample(ind9,n[5])
samp9=sub.a8[sam9,]
index9=1:nrow(sub9)
sample9=sub9[sample(index9,n[5]),]
for (i in 1:n[5]){
	simd[simd$dt==samp9[i,1] & simd$identifier==samp9[i,2],5]=newd9[newd9$dt==sample9[i,1] & 			newd9$identifier==sample9[i,2],5]
}


#get daily mvpa and sedentary time and scale it according to weartime
simd[simd$miss==0,3]=NA
simagg=aggregate(simd$activity,list(simd$dt,simd$identifier),mean,na.rm=TRUE)
names(simagg)=c('dt','identifier','activity')
	mvpa.0=list()
for(i in 1:length(unique(simagg$identifier))){
	x=simd[simd$identifier==unique(simagg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity>1951,3])
	}
	mvpa.0[[i]]=s
}
mvpa.0=unlist(mvpa.0)
simagg$mvpa.0=mvpa.0
sed.0=list()
for(i in 1:length(unique(simagg$identifier))){
	x=simd[simd$identifier==unique(simagg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<100,3])
	}
	sed.0[[i]]=s
}
sed.0=unlist(sed.0)
simagg$sed.0=sed.0
miss=list()
for(i in 1:length(unique(simagg$identifier))){
	x=simd[simd$identifier==unique(simagg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<0,3])
	}
	miss[[i]]=s
}
miss=unlist(miss)
simagg$miss=miss
simagg$mvpa=simagg$mvpa.0-simagg$miss
simagg$sed=simagg$sed.0-simagg$miss
simagg$wt=1440-simagg$miss
simagg$mvpa.adj=simagg$mvpa/simagg$wt*720
simagg$sed.adj=simagg$sed/simagg$wt*720
simag=na.omit(simagg)
simag$prop=simag$miss/1440
simag$identifier=as.character(simag$identifier)



#impute mvpa and sed using mixed model regression
simm2=lme(mvpa~wt,random=~1+wt|identifier,simag,method='REML')
good=simag[simag$prop<0.5,]
bad=simag[simag$prop>=0.5,]
for(i in 1:nrow(bad)){
	name=bad[i,2]
	row=which(rownames(simm2$coefficients$random$identifier)==name)
	slope=simm2$coefficients$random$identifier[row,2]+simm2$coefficients$fixed[2]
	diff=720-bad[i,9]
	act=diff*slope
	bad$mvpa.adj[i]=bad$mvpa[i]+act
}
simm=lme(sed~wt,random=~1+wt|identifier,simag,method='REML')
for(i in 1:nrow(bad)){
	name=bad[i,2]
	row=which(rownames(simm$coefficients$random$identifier)==name)
	slope=simm$coefficients$random$identifier[row,2]+simm$coefficients$fixed[2]
	diff=720-bad[i,9]
	act=diff*slope
	bad$sed.adj[i]=bad$sed[i]+act
}


for(i in 1:nrow(bad)){
	bad$mvpa.adj.true[i]=ag[ag$identifier==bad[i,"identifier"] & ag$dt==bad[i,"dt"],"mvpa.adj"]
	bad$sed.adj.true[i]=ag[ag$identifier==bad[i,"identifier"] & ag$dt==bad[i,"dt"],"sed.adj"]
	
}
setwd("/Users/Selene/Desktop")
save(bad,file="bad.rdata")

#plot true vs imputed mvpa and sed
library("boot")
df1=data.frame(bad$mvpa.adj,bad$mvpa.adj.true)
corr(df1)
plot(bad$mvpa.adj.true,bad$mvpa.adj,pch='.',cex=2.5,ylab='simulated mvpa',xlab='true mvpa')
df2=data.frame(bad$sed.adj,bad$sed.adj.true)
corr(df2)
plot(bad$sed.adj.true,bad$sed.adj,pch='.',cex=2.5,ylab='simulated sedentary time',xlab='true sedentary time')


diff.mvpa=bad$mvpa.adj-bad$mvpa.adj.true
width.mvpa=sd(diff.mvpa)#/sqrt(length(diff.mvpa))
diff.sed=bad$sed.adj-bad$sed.adj.true
width.sed=sd(diff.sed)#/sqrt(length(diff.sed))

plot(bad$mvpa.adj.true,diff.mvpa,pch='.',cex=2.5,ylab='simulated mvpa - true mvpa',xlab='true mvpa')
abline(h=mean(diff.mvpa))
abline(h=mean(diff.mvpa)+width.mvpa*1.96,col='red')
abline(h=mean(diff.mvpa)-width.mvpa*1.96,col='red')
plot(bad$sed.adj.true,diff.sed,pch='.',cex=2.5,ylab='simulated sedentary time - true sedentary time',xlab='true sedentary time')
abline(h=mean(diff.sed))
abline(h=mean(diff.sed)+width.sed*1.96,col='red')
abline(h=mean(diff.sed)-width.sed*1.96,col='red')










