#get number of minutes in sedentary time for every day
load("/Users/Selene/Desktop/Updated Missing Data/extended data with activity and date.rdata")
edata=data.e
agg=aggregate(edata$activity,list(edata$dt,edata$identifier),mean,na.rm=TRUE)
names(agg)=c('dt','identifier','activity')
sed.0=list()
for(i in 1:length(unique(agg$identifier))){
	x=edata[edata$identifier==unique(agg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<100,3])
	}
	sed.0[[i]]=s
}
sed.0=unlist(sed.0)
agg$sed.0=sed.0

miss=list()
for(i in 1:length(unique(agg$identifier))){
	x=edata[edata$identifier==unique(agg$identifier)[i],]
	s=rep(0,length(unique(x$dt)))
	for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<0,3])
	}
	miss[[i]]=s
}
miss=unlist(miss)
agg$miss=miss

agg$sed=agg$sed.0-agg$miss
setwd("/Users/Selene/Desktop")
save(agg,file='aggregate data.rdata')

#scale sed by weartime
agg$wt=1440-agg$miss
agg$sed.adj=agg$sed/agg$wt*720
ag=na.omit(agg)

#read health variables
nv=read.csv('/Users/Selene/Desktop/Updated Missing Data/variables.csv',h=T)
nv=nv[,c(1,3,4,7,2,12,14,13,9,10,11,15,16,17)]
names(nv)=c('identifier','BMI','age','stage','waist','sleep','QOL','insomnia','nei','drink','smoke','mb','depression','education')
save(nv,file='covariates.rdata')
nv$identifier=as.character(nv$identifier)
#nv$arthritis=as.character(nv$arthritis)
nv$depression=as.character(nv$depression)
nv$education=as.character(nv$education)


#get true/reference regression results (we assume the regression parameters estimated using only complete profiles, or profiles with >50% data, are the true/benchmark values)
sub=ag[ag$miss<720,]
sub$identifier=as.character(sub$identifier)
sub$identifier=substr(sub$identifier,4,15)

for(i in 1:nrow(sub)){
	ma=match(sub$identifier[i],nv[,1])
	sub$age[i]=nv[ma,3]
	sub$BMI[i]=nv[ma,2]
	sub$depression[i]=nv[ma,13]
}
sub$dt=as.Date(sub$dt)
sub$dow=weekdays(sub$dt)
sub$ind=ifelse(sub$dow=='Saturday'|sub$dow=='Sunday',1,0)
sub$ind=as.factor(sub$ind)
sub$depression=ifelse(sub$depression=="Don't Know", NA,sub$depression)
sub$depression=as.factor(sub$depression)

library(nlme)
sub=na.omit(sub)
model.true=lme(sed.adj~age+BMI+depression+ind,random=~1|identifier,sub,method='REML')
summary(model.true)











