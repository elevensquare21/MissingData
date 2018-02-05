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
#run 100 simulations, each time simulate missing patterns from the set of "complete" profiles using the pairwise comparison algorithm and then perform regression after EM imputation
#record the results from 100 simulations
##################################################################



#read health variables
nv=read.csv('/Users/Selene/Desktop/Updated Missing Data/variables.csv',h=T)
nv=nv[,c(1,3,4,16)]
names(nv)=c('identifier','BMI','age','depression')
nv$identifier=as.character(nv$identifier)
nv$depression=as.character(nv$depression)

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
m1=data.frame(matrix(rep(0,150), ncol = 15, nrow = 10))
m2=data.frame(matrix(rep(0,150), ncol = 15, nrow = 10))


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
	simagg$wt=1440-simagg$miss
	simagg$mvpa=simagg$mvpa.0-simagg$miss
	simagg$mvpa.adj=simagg$mvpa/simagg$wt*720
	simagg$sed=simagg$sed.0-simagg$miss
	simagg$sed.adj=simagg$sed/simagg$wt*720
	simag=na.omit(simagg)
	simag$miss=ifelse(simag$miss>720,1,0)
	simag$mvpa.adj[which(simag$miss==1)]=NA
	simag$sed.adj[which(simag$miss==1)]=NA


	#perform EM imputation
	len=length(unique(simag$identifier))
	a=as.numeric(table(simag$identifier))
	a=a[-which(a==0)]
	list=list()
	for(i in 1:len){
		vec=1:a[i]
		list[[i]]=vec
	}
	day=unlist(list)
	simag$day=day
	simag$dt=as.Date(simag$dt)
	simag$dow=weekdays(simag$dt)
	simag$ind=ifelse(simag$dow=='Saturday'| simag$dow=='Sunday',1,0)
	simag$identifier=as.character(simag$identifier)
	simag$identifier=substr(simag$identifier,4,15)
	for(i in 1:nrow(simag)){
		ma=match(simag$identifier[i],nv[,1])
		simag$age[i]=nv[ma,3]
		simag$bmi[i]=nv[ma,2]
		simag$depression[i]=nv[ma,4]
	}
	simag$depression=ifelse(simag$depression=="Don't Know", NA,ifelse(simag$depression=='Yes',1,0))

	simag.mvpa=simag[,c("identifier","mvpa.adj","day","ind","age","bmi","depression")]
	a.out.mvpa=amelia(simag.mvpa,m=1,ts='day',cs='identifier')
	simag.imp.mvpa=a.out.mvpa[[1]]$imp1
	simag.imp.mvpa$mvpa.adj=ifelse(simag.imp.mvpa$mvpa.adj<0,0,simag.imp.mvpa$mvpa.adj)
	
	simag.sed=simag[,c("identifier","sed.adj","day","ind","age","bmi","depression")]
	a.out.sed=amelia(simag.sed,m=1,ts='day',cs='identifier')
	simag.imp.sed=a.out.sed[[1]]$imp1
	simag.imp.sed$sed.adj=ifelse(simag.imp.sed$sed.adj<0,0,simag.imp.sed$sed.adj)

	#perform regression
	nvmodel1=lme(mvpa.adj~age+bmi+depression+ind,random=~1|identifier,simag.imp.mvpa,method='REML')
	nvmodel2=lme(sed.adj~age+bmi+depression+ind,random=~1|identifier,simag.imp.sed,method='REML')

	m1[k,1]=summary(nvmodel1)$AIC
	m1[k,2]=summary(nvmodel1)$BIC
	m1[k,3]=summary(nvmodel1)$logLik
	m1[k,4]=summary(nvmodel1)$coefficients[[1]][2]
	m1[k,7]=summary(nvmodel1)$coefficients[[1]][3]
	m1[k,10]=summary(nvmodel1)$coefficients[[1]][4]
	m1[k,5]=summary(nvmodel1)$tTable[2,2]
	m1[k,8]=summary(nvmodel1)$tTable[3,2]
	m1[k,11]=summary(nvmodel1)$tTable[4,2]
	m1[k,6]=summary(nvmodel1)$tTable[2,5]
	m1[k,9]=summary(nvmodel1)$tTable[3,5]
	m1[k,12]=summary(nvmodel1)$tTable[4,5]
	m1[k,13]=summary(nvmodel1)$coefficients[[1]][5]
	m1[k,14]=summary(nvmodel1)$tTable[5,2]
	m1[k,15]=summary(nvmodel1)$tTable[5,5]


	
	m2[k,1]=summary(nvmodel2)$AIC
	m2[k,2]=summary(nvmodel2)$BIC
	m2[k,3]=summary(nvmodel2)$logLik
	m2[k,4]=summary(nvmodel2)$coefficients[[1]][2]
	m2[k,7]=summary(nvmodel2)$coefficients[[1]][3]
	m2[k,10]=summary(nvmodel2)$coefficients[[1]][4]
	m2[k,5]=summary(nvmodel2)$tTable[2,2]
	m2[k,8]=summary(nvmodel2)$tTable[3,2]
	m2[k,11]=summary(nvmodel2)$tTable[4,2]
	m2[k,6]=summary(nvmodel2)$tTable[2,5]
	m2[k,9]=summary(nvmodel2)$tTable[3,5]
	m2[k,12]=summary(nvmodel2)$tTable[4,5]
	m2[k,13]=summary(nvmodel2)$coefficients[[1]][5]
	m2[k,14]=summary(nvmodel2)$tTable[5,2]
	m2[k,15]=summary(nvmodel2)$tTable[5,5]

	print(m1)
}

#save results after every 10 simulations
setwd("/Users/Selene/Desktop")
sim1=list()
sim1[[1]]=m1
sim1[[2]]=m2
save(sim1,file='sim1.rdata')

sim2=list()
sim2[[1]]=m1
sim2[[2]]=m2
save(sim2,file='sim2.rdata')

sim3=list()
sim3[[1]]=m1
sim3[[2]]=m2
save(sim3,file='sim3.rdata')

sim4=list()
sim4[[1]]=m1
sim4[[2]]=m2
save(sim4,file='sim4.rdata')

#combine results from all 100 simulations
m1=rbind(sim1[[1]],sim2[[1]],sim3[[1]],sim4[[1]])	
save(m1,file='m1.rdata')
m2=rbind(sim1[[2]],sim2[[2]],sim3[[2]],sim4[[2]])	
save(m2,file='m2.rdata')


#tabulate model performance in terms of bias,  simulation standard deviation, and mean squared error
model1=data.frame(matrix(rep(0,12), ncol = 4, nrow = 4))
colnames(model1)=c('MSE','Bias','Sim SD','Coverage')
rownames(model1)=c('age','bmi','depression','weekend')
model1[1,1]=sqrt(sum((m1[,4]+0.54238)^2)/100)
model1[1,2]=mean(m1[,4])+0.54238
model1[1,3]=sqrt((model1[1,1])^2-(model1[1,2])^2)
model1[1,4]=sum(ifelse(m1[,4]<0 & m1[,6]<0.01,1,0))/100
model1[2,1]=sqrt(sum((m1[,7]+0.69191)^2)/100)
model1[2,2]=mean(m1[,7])+0.69191
model1[2,3]=sqrt((model1[2,1])^2-(model1[2,2])^2)
model1[2,4]=sum(ifelse(m1[,7]<0 & m1[,9]<0.01,1,0))/100
model1[3,1]=sqrt(sum((m1[,10]+4.70393)^2)/100)
model1[3,2]=mean(m1[,10])+4.70393
model1[3,3]=sqrt((model1[3,1])^2-(model1[3,2])^2)
model1[3,4]=sum(ifelse(m1[,10]<0 & m1[,12]<0.01,1,0))/100
model1[4,1]=sqrt(sum((m1[,13]-0.10076)^2)/100)
model1[4,2]=mean(m1[,13])-0.10076
model1[4,3]=sqrt((model1[4,1])^2-(model1[4,2])^2)
model1[4,4]=sum(ifelse(m1[,13]>0 & m1[,15]>0.5,1,0))/100
model.imp.mvpa=model1
save(model.imp.mvpa,file='mdel.imp.mvpa result.rdata')

model2=data.frame(matrix(rep(0,12), ncol = 4, nrow = 4))
colnames(model2)=c('MSE','Bias','Sim SD','Coverage')
rownames(model2)=c('age','bmi','depression','weekend')
model2[1,1]=sqrt(sum((m2[,4]-1.97855)^2)/100)
model2[1,2]=mean(m2[,4])-1.97855
model2[1,3]=sqrt((model2[1,1])^2-(model2[1,2])^2)
model2[1,4]=sum(ifelse(m2[,4]>0 & m2[,6]<0.01,1,0))/100
model2[2,1]=sqrt(sum((m2[,7]-1.79275)^2)/100)
model2[2,2]=mean(m2[,7])-1.79275
model2[2,3]=sqrt((model2[2,1])^2-(model2[2,2])^2)
model2[2,4]=sum(ifelse(m2[,7]>0 & m2[,9]<0.05,1,0))/100
model2[3,1]=sqrt(sum((m2[,10]-7.16928)^2)/100)
model2[3,2]=mean(m2[,10])-7.16928
model2[3,3]=sqrt((model2[3,1])^2-(model2[3,2])^2)
model2[3,4]=sum(ifelse(m2[,10]>0 & m2[,12]<0.5,1,0))/100
model2[4,1]=sqrt(sum((m2[,13]+8.85243)^2)/100)
model2[4,2]=mean(m2[,13])+8.85243
model2[4,3]=sqrt((model2[4,1])^2-(model2[4,2])^2)
model2[4,4]=sum(ifelse(m2[,13]<0 & m2[,15]<0.01,1,0))/100
save(model2,file='m2 result.rdata')

model.imp.sed=model2
save(model.imp.sed,file='mdel.imp.sed result.rdata')











