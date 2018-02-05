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
#run 100 simulations, each time simulate missing patterns from the set of "complete" profiles using the pairwise comparison algorithm and then apply method 3 (k nearest neighbor)
#record the results from 100 simulations
##################################################################

#read health variables
nv=read.csv('/Users/Selene/Desktop/Updated Missing Data/variables.csv',h=T)
nv=nv[,c(1,3,4,17)]
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
no=round(perc*nrow(sub))


#run 10 simulations at a time and save results for every 10 simluations
knn=data.frame(matrix(rep(0,150), ncol = 15, nrow = 10))

for (k in 1:10){
	#start with the set of complete profiles (name is newd), choose an appropriate number of days to become days with 50%-60% time missing
	simd=newd
	ind5=1:nrow(sub)
	sam5=sample(ind5,no[1])
	samp5=sub[sam5,]
	index5=1:nrow(sub5)
	sample5=sub5[sample(index5,no[1]),]
	for (i in 1:no[1]){
		simd[simd$dt==samp5[i,1] & simd$identifier==samp5[i,2],5]=newd5[newd5$dt==sample5[i,1] & 			newd5$identifier==sample5[i,2],5]
	}
	
	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 60%-70% time missing
	sub.a5=sub[-sam5,]
	ind6=1:nrow(sub.a5)
	sam6=sample(ind6,no[2])
	samp6=sub.a5[sam6,]
	index6=1:nrow(sub6)
	sample6=sub6[sample(index6,no[2]),]
	for (i in 1:no[2]){
		simd[simd$dt==samp6[i,1] & simd$identifier==samp6[i,2],5]=newd6[newd6$dt==sample6[i,1] & 			newd6$identifier==sample6[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 70%-80% time missing
	sub.a6=sub.a5[-sam6,]
	ind7=1:nrow(sub.a6)
	sam7=sample(ind7,no[3])
	samp7=sub.a6[sam7,]
	index7=1:nrow(sub7)
	sample7=sub7[sample(index7,no[3]),]
	for (i in 1:no[3]){
		simd[simd$dt==samp7[i,1] & simd$identifier==samp7[i,2],5]=newd7[newd7$dt==sample7[i,1] & 			newd7$identifier==sample7[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 80%-90% time missing
	sub.a7=sub.a6[-sam7,]
	ind8=1:nrow(sub.a7)
	sam8=sample(ind8,no[4])
	samp8=sub.a7[sam8,]
	index8=1:nrow(sub8)
	sample8=sub8[sample(index8,no[4]),]
	for (i in 1:no[4]){
		simd[simd$dt==samp8[i,1] & simd$identifier==samp8[i,2],5]=newd8[newd8$dt==sample8[i,1] & 			newd8$identifier==sample8[i,2],5]
	}

	#from what's left in the set of complete profiles, choose an appropriate number of days to become days with 90%-100% time missing
	sub.a8=sub.a7[-sam8,]
	ind9=1:nrow(sub.a8)
	sam9=sample(ind9,no[5])
	samp9=sub.a8[sam9,]
	index9=1:nrow(sub9)
	sample9=sub9[sample(index9,no[5]),]
	for (i in 1:no[5]){
		simd[simd$dt==samp9[i,1] & simd$identifier==samp9[i,2],5]=newd9[newd9$dt==sample9[i,1] & 			newd9$identifier==sample9[i,2],5]
	}
	
	#use knn method to impute missing data
	simd[simd$miss==0,3]=NA
	simd=simd[,c(1,2,3)]
	nd=nrow(simd)/1440
	s=rep(0,nd)
	for (i in 1:nd){
		m=simd[(i-1)*1440+1:1440,'activity']
		s[i]=sum(is.na(m))
	}

	wh=which(s>1400)
	list=list()
	for(i in 1:length(wh)){
		n=wh[i]
		list[[i]]=(n-1)*1440+1:1440
	}
	l=unlist(list)
	simd=simd[-l,]

	nd=nrow(simd)/1440
	s=rep(0,nd)
	for (i in 1:nd){
		m=simd[(i-1)*1440+1:1440,'activity']
		s[i]=sum(is.na(m))
	}

	impute=function(a, a.impute){
		ifelse(is.na(a),a.impute,a)
	}
	w=which(s>720)
	f=which(s<=720)
	for (i in 1:length(w)){
		n=w[i]
		m=simd[(n-1)*1440+1:1440,'activity']
		d=rep(0,length(f))
		for (j in 1:length(f)){
			q=f[j]
			ma=simd[(q-1)*1440+1:1440,'activity']
			d[j]=sum((m-ma)^2,na.rm=TRUE)/sum(!is.na(m+ma))
		}
		cand=which(d<=sort(d)[5])
		sam=sample(cand,1)
		p=f[sam]
		imp=simd[(p-1)*1440+1:1440,'activity']
		simd[(n-1)*1440+1:1440,'activity']=impute(m,imp)
	}

	#reduce minute level data to daily summary data and run regression
	simagg=aggregate(simd$activity,list(simd$dt,simd$identifier),mean,na.rm=TRUE)
	names(simagg)=c('dt','identifier','activity')
	simag=na.omit(simagg)
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
	simag=na.omit(simag)
	mod=lme(activity~age+bmi+depression+ind,random=~1|identifier,simag,method='REML')
	
	knn[k,1]=summary(mod)$AIC
	knn[k,2]=summary(mod)$BIC
	knn[k,3]=summary(mod)$logLik
	knn[k,4]=summary(mod)$coefficients[[1]][2]
	knn[k,7]=summary(mod)$coefficients[[1]][3]
	knn[k,10]=summary(mod)$coefficients[[1]][4]
	knn[k,5]=summary(mod)$tTable[2,2]
	knn[k,8]=summary(mod)$tTable[3,2]
	knn[k,11]=summary(mod)$tTable[4,2]
	knn[k,6]=summary(mod)$tTable[2,5]
	knn[k,9]=summary(mod)$tTable[3,5]
	knn[k,12]=summary(mod)$tTable[4,5]
	knn[k,13]=summary(mod)$coefficients[[1]][5]
	knn[k,14]=summary(mod)$tTable[5,2]
	knn[k,15]=summary(mod)$tTable[5,5]

	print(knn)
	
}

#save results after every 10 simulations
setwd("/Users/Selene/Desktop")	
sim1=knn
save(sim1,file='sim1.rdata')

sim2=knn
save(sim2,file='sim2.rdata')

sim3=knn
save(sim3,file='sim3.rdata')

sim4=knn
save(sim4,file='sim4.rdata')

sim5=knn
save(sim5,file='sim5.rdata')

sim6=knn
save(sim6,file='sim6.rdata')

sim7=knn
save(sim7,file='sim7.rdata')

sim8=knn
save(sim8,file='sim8.rdata')

sim9=knn
save(sim9,file='sim9.rdata')

sim10=knn
save(sim10,file='sim10.rdata')

#combine results from all 100 simulations
knn.result=rbind(sim1,sim2,sim3,sim4,sim5,sim6,sim7,sim8,sim9,sim10)
save(knn.result,file='knn.result.rdata')

#tabulate model performance in terms of bias,  simulation standard deviation, and mean squared error
knnmodel=data.frame(matrix(rep(0,12), ncol = 4, nrow = 4))
colnames(knnmodel)=c('MSE','Bias','Sim SD','Coverage')
rownames(knnmodel)=c('age','bmi','depression','weekend')
knnmodel[1,1]=sqrt(sum((knn.result[,4]+4.6003)^2)/100)
knnmodel[1,2]=mean(knn.result[,4])+4.6003
knnmodel[1,3]=sqrt((knnmodel[1,1])^2-(knnmodel[1,2])^2)
knnmodel[1,4]=sum(ifelse(knn.result[,4]<0 & knn.result[,6]<0.01,1,0))/100
knnmodel[2,1]=sqrt(sum((knn.result[,7]+4.1768)^2)/100)
knnmodel[2,2]=mean(knn.result[,7])+4.1768
knnmodel[2,3]=sqrt((knnmodel[2,1])^2-(knnmodel[2,2])^2)
knnmodel[2,4]=sum(ifelse(knn.result[,7]<0 & knn.result[,9]<0.01,1,0))/100
knnmodel[3,1]=sqrt(sum((knn.result[,10]+30.9295)^2)/100)
knnmodel[3,2]=mean(knn.result[,10])+30.9295
knnmodel[3,3]=sqrt((knnmodel[3,1])^2-(knnmodel[3,2])^2)
knnmodel[3,4]=sum(ifelse(knn.result[,10]<0 & knn.result[,12]<0.01,1,0))/100
knnmodel[4,1]=sqrt(sum((knn.result[,13]-8.3622)^2)/100)
knnmodel[4,2]=mean(knn.result[,13])-8.3622
knnmodel[4,3]=sqrt((knnmodel[4,1])^2-(knnmodel[4,2])^2)
knnmodel[4,4]=sum(ifelse(knn.result[,13]>0 & knn.result[,15]<0.1,1,0))/100
save(knnmodel,file='knnmodel.rdata')














