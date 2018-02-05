load("/Users/Selene/Desktop/Updated Missing Data/extended data with activity and date.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/aggregate on mean.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with more than 50% time.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 50%-60% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 60%-70% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 70%-80% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 80%-90% time missing.rdata")
load("/Users/Selene/Desktop/Updated Missing Data/days with 90%-100% time missing.rdata")
library(nlme)

##################################################################
##################################################################
#run 100 simulations, each time simulate missing patterns from the set of "complete" profiles using the pairwise comparison algorithm and then perform regression with weartime adjusted
#record the results from 100 simulations
##################################################################



#read health variables
nv=read.csv('/Users/Selene/Desktop/Updated Missing Data/variables.csv',h=T)
nv=nv[,c(1,3,4,16)]
names(nv)=c('identifier','BMI','age','depression')
save(nv,file='covariates.rdata')
nv$identifier=as.character(nv$identifier)
nv$depression=as.character(nv$depression)
save(nv,file='covariates.rdata')

#save number of profiles in each stratum of "missingness". Used for simulating missing patterns from "complete" profiles later
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

#run 10 simulations at a time and save results for every 10 simluations
m0=data.frame(matrix(rep(0,150), ncol = 15, nrow = 10))

for (k in 1:10){
	#start with the set of complete profiles (name is newd), choose an appropriate number of days to become days with 50%-60% time missing
	simd=newd
	ind5=1:nrow(sub)
	sam5=sample(ind5,n[1])
	samp5=sub[sam5,]
	index5=1:nrow(sub5)
	sample5=sub5[sample(index5,n[1]),]
	for (i in 1:n[1]){
		simd[simd$dt==samp5[i,1] & simd$identifier==samp5[i,2],5]=newd5[newd5$dt==sample5[i,1] & 			newd5$identifier==sample5[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 60%-70% time missing	
	sub.a5=sub[-sam5,]
	ind6=1:nrow(sub.a5)
	sam6=sample(ind6,n[2])
	samp6=sub.a5[sam6,]
	index6=1:nrow(sub6)
	sample6=sub6[sample(index6,n[2]),]
	for (i in 1:n[2]){
		simd[simd$dt==samp6[i,1] & simd$identifier==samp6[i,2],5]=newd6[newd6$dt==sample6[i,1] & 			newd6$identifier==sample6[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 70%-80% time missing
	sub.a6=sub.a5[-sam6,]
	ind7=1:nrow(sub.a6)
	sam7=sample(ind7,n[3])
	samp7=sub.a6[sam7,]
	index7=1:nrow(sub7)
	sample7=sub7[sample(index7,n[3]),]
	for (i in 1:n[3]){
		simd[simd$dt==samp7[i,1] & simd$identifier==samp7[i,2],5]=newd7[newd7$dt==sample7[i,1] & 			newd7$identifier==sample7[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 80%-90% time missing
	sub.a7=sub.a6[-sam7,]
	ind8=1:nrow(sub.a7)
	sam8=sample(ind8,n[4])
	samp8=sub.a7[sam8,]
	index8=1:nrow(sub8)
	sample8=sub8[sample(index8,n[4]),]
	for (i in 1:n[4]){
		simd[simd$dt==samp8[i,1] & simd$identifier==samp8[i,2],5]=newd8[newd8$dt==sample8[i,1] & 			newd8$identifier==sample8[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 90%-100% time missing
	sub.a8=sub.a7[-sam8,]
	ind9=1:nrow(sub.a8)
	sam9=sample(ind9,n[5])
	samp9=sub.a8[sam9,]
	index9=1:nrow(sub9)
	sample9=sub9[sample(index9,n[5]),]
	for (i in 1:n[5]){
		simd[simd$dt==samp9[i,1] & simd$identifier==samp9[i,2],5]=newd9[newd9$dt==sample9[i,1] & 			newd9$identifier==sample9[i,2],5]
	}
	
	#reduce minute level data to daily summary data and run regression with weartime adjusted
	simd[simd$miss==0,3]=NA
	simagg=aggregate(simd$activity,list(simd$dt,simd$identifier),mean,na.rm=TRUE)
	names(simagg)=c('dt','identifier','activity')
	pro=list()
	for(i in 1:length(unique(simagg$identifier))){
		x=simd[simd$identifier==unique(simagg$identifier)[i],]
		s=rep(0,length(unique(x$dt)))
		for(j in 1:length(unique(x$dt))){
		s[j]=length(x[x$dt==unique(x$dt)[j] & x$activity<0,3])/length(x[x$dt==unique(x$dt)[j],3])
		}
		pro[[i]]=s
	}
	prop=unlist(pro)
	simagg$prop=prop
	simag=na.omit(simagg)
	simag$weights=1/(1-simag$prop)
	simag$identifier=as.character(simag$identifier)
	simag$identifier=substr(simag$identifier,4,15)
	for(i in 1:nrow(simag)){
		ma=match(simag$identifier[i],nv[,1])
		simag$age[i]=nv[ma,3]
		simag$bmi[i]=nv[ma,2]
		simag$depression[i]=nv[ma,4]
	}
	simag$depression=ifelse(simag$depression=="Don't Know", NA,simag$depression)
	simag$depression=as.factor(simag$depression)
	simag$dt=as.Date(simag$dt)
	simag$dow=weekdays(simag$dt)
	simag$ind=ifelse(simag$dow=='Saturday'|simag$dow=='Sunday',1,0)
	simag$ind=as.factor(simag$ind)
	simag$wt=(1-simag$prop)*1440

	simag=na.omit(simag)


	nvmodel=lme(activity~age+bmi+depression+ind+wt,random=~1|identifier,simag,method='REML')

	m0[k,1]=summary(nvmodel)$AIC
	m0[k,2]=summary(nvmodel)$BIC
	m0[k,3]=summary(nvmodel)$logLik
	m0[k,4]=summary(nvmodel)$coefficients[[1]][2]
	m0[k,7]=summary(nvmodel)$coefficients[[1]][3]
	m0[k,10]=summary(nvmodel)$coefficients[[1]][4]
	m0[k,5]=summary(nvmodel)$tTable[2,2]
	m0[k,8]=summary(nvmodel)$tTable[3,2]
	m0[k,11]=summary(nvmodel)$tTable[4,2]
	m0[k,6]=summary(nvmodel)$tTable[2,5]
	m0[k,9]=summary(nvmodel)$tTable[3,5]
	m0[k,12]=summary(nvmodel)$tTable[4,5]
	m0[k,13]=summary(nvmodel)$coefficients[[1]][5]
	m0[k,14]=summary(nvmodel)$tTable[5,2]
	m0[k,15]=summary(nvmodel)$tTable[5,5]
	
	print(m0)
}


#save results after every 10 simulations
setwd("/Users/Selene/Desktop")
sim1=m0
save(sim1,file='sim1.rdata')

sim2=m0
save(sim2,file='sim2.rdata')

sim3=m0
save(sim3,file='sim3.rdata')

sim4=m0
save(sim4,file='sim4.rdata')

sim5=m0
save(sim5,file='sim5.rdata')

sim6=m0
save(sim6,file='sim6.rdata')

#combine results from all 100 simulations
m00=rbind(sim1,sim2,sim3,sim4,sim5,sim6)
save(m00,file='m00.rdata')

#tabulate model performance in terms of bias,  simulation standard deviation, and mean squared error
m0=m00
model0=data.frame(matrix(rep(0,12), ncol = 4, nrow = 4))
colnames(model0)=c('MSE','Bias','Sim SD','Coverage')
rownames(model0)=c('age','bmi','depression','weekend')
model0[1,1]=sqrt(sum((m0[,4]+4.6003)^2)/100)
model0[1,2]=mean(m0[,4])+4.6003
model0[1,3]=sqrt((model0[1,1])^2-(model0[1,2])^2)
model0[1,4]=sum(ifelse(m0[,4]<0 & m0[,6]<0.01,1,0))/100
model0[2,1]=sqrt(sum((m0[,7]+4.1768)^2)/100)
model0[2,2]=mean(m0[,7])+4.1768
model0[2,3]=sqrt((model0[2,1])^2-(model0[2,2])^2)
model0[2,4]=sum(ifelse(m0[,7]<0 & m0[,9]<0.01,1,0))/100
model0[3,1]=sqrt(sum((m0[,10]+30.9295)^2)/100)
model0[3,2]=mean(m0[,10])+30.9295
model0[3,3]=sqrt((model0[3,1])^2-(model0[3,2])^2)
model0[3,4]=sum(ifelse(m0[,10]<0 & m0[,12]<0.01,1,0))/100
model0[4,1]=sqrt(sum((m0[,13]-8.3622)^2)/100)
model0[4,2]=mean(m0[,13])-8.3622
model0[4,3]=sqrt((model0[4,1])^2-(model0[4,2])^2)
model0[4,4]=sum(ifelse(m0[,13]>0 & m0[,15]<0.1,1,0))/100
adjust.wt=model0
save(adjust.wt,file='adjust.wt.rdata')

























