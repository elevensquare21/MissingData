setwd("/Users/Selene/Desktop")


#read raw data
data.11=read.csv("/Users/Selene/Desktop/RfH_accgps_21050701/RfH_A1_2011_valid.csv",h=T)
data.12=read.csv("/Users/Selene/Desktop/RfH_accgps_21050701/RfH_A1_2012_valid.csv",h=T)
data.13=read.csv("/Users/Selene/Desktop/RfH_accgps_21050701/RfH_A1_2013_valid.csv",h=T)
data.14=read.csv("/Users/Selene/Desktop/RfH_accgps_21050701/RfH_A1_2014_valid.csv",h=T)
data.15=read.csv("/Users/Selene/Desktop/RfH_accgps_21050701/RfH_A1_2015_valid.csv",h=T)
data.11$identifier=substr(data.11$identifier,1,15)
data.12$identifier=substr(data.12$identifier,1,15)
data.13$identifier=substr(data.13$identifier,1,15)
data.14$identifier=substr(data.14$identifier,1,15)
data.15$identifier=substr(data.15$identifier,1,15)
fulldat=rbind(data.11,data.12,data.13,data.14,data.15)
fulldat$dt=substr(fulldat$dateTime,1,10)
fulldat$time=substr(fulldat$dateTime,12,19)
data.f=fulldat[,c('identifier','dt','activity','time')]
data.f$activity[which(data.f$activity<0)]=NA
save(data.f,file='fulldata.rdata')


#complete times of day (some days don't have the full 1440min record, fill the missing times with activity=NA)
mdat=aggregate(data.f$activity,list(data.f$dt,data.f$identifier),mean,na.rm=TRUE)
names(mdat)=c('dt','identifier','activity')

t1=head(data.f,1440)[,4]
time=rep(t1,nrow(mdat))

l=list()
for(i in 1:length(unique(mdat$identifier))){
	n=sum(mdat$identifier==unique(mdat$identifier)[i])
	l[[i]]=rep(unique(mdat$identifier)[i],n*1440)
}
identifier=unlist(l)

dt=rep(mdat$dt,each=1440)

lis=list()
for(i in 1:length(unique(mdat$identifier))){
	x=data.f[data.f$identifier==unique(mdat$identifier)[i],]
	list=list()
	for(j in 1:length(unique(x$dt))){
		match=match(t1,x[x$dt==unique(x$dt)[j],4])
		match[!is.na(match)]=x[x$dt==unique(x$dt)[j],3]
		list[[j]]=match
	}
	lis[[i]]=unlist(list)
}
activity=unlist(lis)

data.e=data.frame(identifier,dt,activity,time)
save(data.e,file='extended data with activity and date.rdata')


#get proportion missing data for every day
edata=data.e
agg=aggregate(edata$activity,list(edata$dt,edata$identifier),mean,na.rm=TRUE)
names(agg)=c('dt','identifier','activity')
pro=list()
for(i in 1:length(unique(agg$identifier))){
	x=edata[edata$identifier==unique(agg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<0,3])/length(x[x$dt==unique(x$dt)[j],3])
	}
	pro[[i]]=s
}
prop=unlist(pro)
agg$prop=prop
ag=na.omit(agg)
save(ag,file='aggregate on mean.rdata')



#get days with >50% data, days with 50%-60% time missing, days with 60%-70% time missing, days with 70%-80% time missing, days with 80%-90% time missing, days with >90% time missing
miss=ifelse(edata$activity=='NA',0,1)
miss[is.na(miss)]=0
edata$miss=miss

sub=ag[ag$prop<0.5,]
sub5=ag[ag$prop>=0.5 & ag$prop<0.6,]
sub6=ag[ag$prop>=0.6 & ag$prop<0.7,]
sub7=ag[ag$prop>=0.7 & ag$prop<0.8,]
sub8=ag[ag$prop>=0.8 & ag$prop<0.9,]
sub9=ag[ag$prop>=0.9 & ag$prop<1,]

library(plyr)
#sub
list=list()
for (i in 1:length(unique(sub$identifier))){
	x=edata[edata$identifier==unique(sub$identifier)[i],]	
	y=sub[sub$identifier==unique(sub$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd=ldply(list,data.frame)
save(newd,file='days with more than 50% time.rdata')

#sub5
list=list()
for (i in 1:length(unique(sub5$identifier))){
	x=edata[edata$identifier==unique(sub5$identifier)[i],]	
	y=sub5[sub5$identifier==unique(sub5$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd5=ldply(list,data.frame)
save(newd5,file='days with 50%-60% time missing.rdata')

#sub6
list=list()
for (i in 1:length(unique(sub6$identifier))){
	x=edata[edata$identifier==unique(sub6$identifier)[i],]	
	y=sub6[sub6$identifier==unique(sub6$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd6=ldply(list,data.frame)
save(newd6,file='days with 60%-70% time missing.rdata')

#sub7
list=list()
for (i in 1:length(unique(sub7$identifier))){
	x=edata[edata$identifier==unique(sub7$identifier)[i],]	
	y=sub7[sub7$identifier==unique(sub7$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd7=ldply(list,data.frame)
save(newd7,file='days with 70%-80% time missing.rdata')

#sub8
list=list()
for (i in 1:length(unique(sub8$identifier))){
	x=edata[edata$identifier==unique(sub8$identifier)[i],]	
	y=sub8[sub8$identifier==unique(sub8$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd8=ldply(list,data.frame)
save(newd8,file='days with 80%-90% time missing.rdata')

#sub9
list=list()
for (i in 1:length(unique(sub9$identifier))){
	x=edata[edata$identifier==unique(sub9$identifier)[i],]	
	y=sub9[sub9$identifier==unique(sub9$identifier)[i],]
	l=list()
	for(j in 1:length(unique(y$dt))){
		z=x[x$dt==unique(y$dt)[j],]
		l[[j]]=z
	}
	li=ldply(l,data.frame)
	list[[i]]=li
}
newd9=ldply(list,data.frame)
save(newd9,file='days with 90%-100% time missing.rdata')


#read health variables 
nv=read.csv('/Users/Selene/Desktop/Updated Missing Data/variables.csv',h=T)
nv=nv[,c(1,3,4,7,2,12,14,13,9,10,11,15,16,17,18)]
names(nv)=c('identifier','BMI','age','stage','waist','sleep','QOL','insomnia','nei','drink','smoke','mb','arthritis','depression','education')
save(nv,file='covariates.rdata')
nv$identifier=as.character(nv$identifier)
nv$arthritis=as.character(nv$arthritis)
nv$depression=as.character(nv$depression)
nv$education=as.character(nv$education)


#get true/reference regression results (we assume the regression parameters estimated using only complete profiles, or profiles with >50% data, are the true/benchmark values)
sub=ag[ag$prop<0.5,]

sub$identifier=as.character(sub$identifier)
sub$identifier=substr(sub$identifier,4,15)
for(i in 1:nrow(sub)){
	ma=match(sub$identifier[i],nv[,1])
	sub$age[i]=nv[ma,3]
	sub$BMI[i]=nv[ma,2]
	sub$stage[i]=nv[ma,4]
	sub$waist[i]=nv[ma,5]
	sub$sleep[i]=nv[ma,6]
	sub$QOL[i]=nv[ma,7]
	sub$insomnia[i]=nv[ma,8]
	sub$nei[i]=nv[ma,9]
	sub$mb[i]=nv[ma,12]
	sub$arthritis[i]=nv[ma,13]
	sub$depression[i]=nv[ma,14]
	sub$education[i]=nv[ma,15]
	
}
sub$stage=as.factor(sub$stage)
sub$dt=as.Date(sub$dt)
sub$dow=weekdays(sub$dt)
sub$ind=ifelse(sub$dow=='Saturday'|sub$dow=='Sunday',1,0)
sub$ind=as.factor(sub$ind)

sub$bmi.cat=ifelse(sub$BMI<30,0,1)
sub$bmi.cat=as.factor(sub$bmi.cat)
sub$qol.cat=ifelse(sub$QOL<6,0,1)
sub$qol.cat=as.factor(sub$qol.cat)
sub$sleep.cat=ifelse(sub$sleep<7,0,1)
sub$sleep.cat=as.factor(sub$sleep.cat)
sub$insomnia=as.factor(sub$insomnia)
sub$nei=as.factor(sub$nei)
sub$arthritis=ifelse(sub$arthritis=="Don't Know", NA,sub$arthritis)
sub$arthritis=as.factor(sub$arthritis)
sub$education=ifelse(sub$education=="8th grade or less" | sub$education=="Completed some high school but did not graduate" | sub$education=="Graduated from  high school or G.E.D.",0,1)
sub$education=as.factor(sub$education)
sub$depression=ifelse(sub$depression=="Don't Know", NA,sub$depression)
sub$depression=as.factor(sub$depression)
sub$stage

library(nlme)
sub=na.omit(sub)
model.true=lme(activity~age+BMI+depression,random=~1|identifier,sub,method='REML')
summary(model.true)

m0=lme(activity~age+BMI+depression+ind,random=~1|identifier,sub,method='REML')
plot(m0,resid(.)~prop,abline=0,xlab='Proportion of Missing Data in a Day')


#plot proportion of days during which data is present over time of day
edata=data.e
nd=nrow(edata)/1440
propv=rep(0,1440)
for(i in 1:1440){
	vec=rep(0,nd)
	for(j in 1:nd){
		vec[j]=edata[(j-1)*1440+i,3]
	}
	propv[i]=(nd-sum(is.na(vec)))/nd
}

plot(com$min,propv,type='l',xlab='time of day',ylab='proportion of days during which accelerometer is worn')


#generate an example to show how missing pattern was simulated
com=data.e[1:1440,]
com$min=as.POSIXct(com$time,format='%H:%M:%S')
plot(com$min, com$activity,type='l',main='Complete Profile',xlab='time',ylab='activity')
com$act2=com$activity
com$act2[1:660]=NA
com$act2[782:1440]=NA
com$act3=com$activity
com$act3[1:540]=NA
com$act3[1202:1440]=NA
par(mfrow=c(2,1))
plot(com$min, com$activity,type='l',main='Complete Profile',xlab='time',ylab='activity',ylim=c(0,4000))
plot(com$min, com$act3,type='l',main='Profile with Simulated Missing Data',xlab='time',ylab='activity',ylim=c(0,4000))

























