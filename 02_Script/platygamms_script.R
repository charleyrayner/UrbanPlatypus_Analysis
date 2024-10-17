library(lme4)
library(AICcmodavg)
#download package "mgcv" (generalised addditive mixed model GAMMs)
library(mgcv)
library(gamm4)
library(AICcmodavg)
library(tidyverse)

# Load functions:
invisible(lapply(paste("01_Functions/",dir("01_Functions"),sep=""), function(x) source(x)))

# Load and format platy data
platystatCatchment<-read.table("00_Data/platystatCatchment.txt",header=T)
platystatCatchment$siteID<-as.factor(platystatCatchment$siteID)
platystatCatchment$catchment<-as.factor(platystatCatchment$catchment)
platystatCatchment$year<-platystatCatchment$year-min(platystatCatchment$year)
head(platystatCatchment);dim(platystatCatchment)

unique(platystatCatchment$siteID)
unique(platystatCatchment$year)
str(platystatCatchment)

# Visualise platypus occurrence probability across years
head(platystatCatchment);dim(platystatCatchment)

xx<-table(platystatCatchment$year,platystatCatchment$occurrence)
xx[,2]/(xx[,1]+xx[,2])
prop1<-xx[,2]/(xx[,1]+xx[,2])
plot(0:10,prop1)
plot(0:10,prop1,ylim=c(0,1))


# GAMM info
# the k term cannot be more than 10 (10 years)

# CANDIDATE SELECTION 5mMrf (5-month prior mean rainfall) vs 5mTrf (5-month prior total rainfall) vs year

gamm.5mMrf <- gamm4(occurrence~t2(X5mMrf,k=10),random=~(1|siteID/catchment),data=platystatCatchment,family=binomial)
summary(gamm.5mMrf$gam)

gamm.5mTrf <- gamm4(occurrence~t2(X5mTrf,k=10),random=~(1|siteID/catchment),data=platystatCatchment,family=binomial)
summary(gamm.5mTrf$gam)

gamm.year <- gamm4(occurrence ~ t2(year, k=10), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(gamm.year$gam)

gamm.NULL <- gamm4(occurrence ~ 1, random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(gamm.NULL$gam)

library(AICcmodavg)
gamm.cand<-list(NDBI.5mMrf$mer,NDBI.5mTrf$mer,gamm.year$mer,gamm.NULL$mer)
gamm.names<-c("5mMrf","5mTrf","year only","NULL")
gamm.tab<-aictab(gamm.cand,modnames = gamm.names,second.ord = T,sort = T)
gamm.tab
# year determined to be have lowest AICc so will be using year as temporal variation 

                 
# NDBI CANDIDATE GAMM MODELS
NDBI.int <- gamm4(occurrence ~ t2(year, k=10) + t2(ndbi, k=10, bs="fs") + t2(year, ndbi, k=10, bs=c("tp", "fs")), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDBI.int$gam)

NDBI.add <- gamm4(occurrence ~ t2(year, k=10) + t2(ndbi, k=10, bs="fs"), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDBI.add$gam)

NDBI.ndbi <- gamm4(occurrence ~ t2(ndbi, k=10, bs="fs"), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDBI.ndbi$gam)

NDBI.cand<-list(NDBI.int$mer,NDBI.add$mer,gamm.year$mer,NDBI.ndbi$mer,gamm.NULL$mer)
NDBI.names<-c("interaction","additive","year only","ndbi only","NULL")
NDBI.tab<-aictab(NDBI.cand,modnames = NDBI.names,second.ord = T,sort = T)
NDBI.tab

head(platystatCatchment);dim(platystatCatchment)

summary(platystatCatchment$ndbi)

#plotting ndbi in different years
seq(min(platystatCatchment$ndbi,na.rm=T),max(platystatCatchment$ndbi,na.rm=T),length.out=50)

ndbi.sum<-summary(platystatCatchment$ndbi)[c("Min.","1st Qu.","Mean","Max.")]
newdf1 <- data.frame(year=rep(min(platystatCatchment$year):max(platystatCatchment$year),length(ndbi.sum)))
newdf1$ndbi<-rep(ndbi.sum,rep(length(unique(newdf1$year)),length(ndbi.sum)))
newdf1$type<-rep(names(ndbi.sum),rep(length(unique(newdf1$year)),length(ndbi.sum)))
newdf2<-newdf1
ndbi.pr<-predict(NDBI.int$gam,newdata=newdf1,se.fit=T,type='response')
newdf1$fit<-ndbi.pr$fit
newdf1$se.fit<-ndbi.pr$se.fit
newdf1$lci <- newdf1$fit-(newdf1$se.fit*1.96)
newdf1$uci <- newdf1$fit+(newdf1$se.fit*1.96)
str(ndbi.pr)

head(newdf1);dim(newdf1)



#############################
# Modify 
install.packages("arm")
library(arm)
??arm
newdf1 <- data.frame(year=rep(min(platystatCatchment$year):max(platystatCatchment$year),length(ndbi.sum)))
newdf1$ndbi<-rep(ndbi.sum,rep(length(unique(newdf1$year)),length(ndbi.sum)))
newdf1$type<-rep(names(ndbi.sum),rep(length(unique(newdf1$year)),length(ndbi.sum)))
newdf2a<-newdf1
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad2<-predict(NDBI.int$gam, newdata = newdf2a, se.fit = T, type="link")
newdf2<-data.frame(newdf2, fit.link=pr_quad2$fit, se.link=pr_quad2$se.fit)
newdf2$lci.link<-newdf2$fit-(newdf2$se*1.96)
newdf2$uci.link<-newdf2$fit+(newdf2$se*1.96)

newdf2$fit<-invlogit(newdf2$fit.link)
newdf2$se<-invlogit(newdf2$se.link)
newdf2$lci<-invlogit(newdf2$lci.link)
newdf2$uci<-invlogit(newdf2$uci.link)
# predict on the link scale (logit scale) 
#########################


# NDBI only model (second-rank model)
summary(NDBI.ndbi$gam)

newdf3a<-data.frame(ndbi=seq(min(platystatCatchment$ndbi,na.rm=T),max(platystatCatchment$ndbi,na.rm=T),length.out=50))
head(newdf3a);dim(newdf3a)
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad3<-predict(NDBI.ndbi$gam, newdata = newdf3a, se.fit = T, type="link")
newdf3<-data.frame(newdf3a, fit.link=pr_quad3$fit, se.link=pr_quad3$se.fit)
newdf3$lci.link<-newdf3$fit-(newdf3$se*1.96)
newdf3$uci.link<-newdf3$fit+(newdf3$se*1.96)

newdf3$fit<-invlogit(newdf3$fit.link)
newdf3$se<-invlogit(newdf3$se.link)
newdf3$lci<-invlogit(newdf3$lci.link)
newdf3$uci<-invlogit(newdf3$uci.link)
head(newdf3,3);dim(newdf3)
head(newdf2,3);dim(newdf2)
newdf2.min <- subset(newdf2, newdf2$type=='Min.')
rownames(newdf2.min)<-1:nrow(newdf2.min)
newdf2.mean <- subset(newdf2, newdf2$type=='Mean')
rownames(newdf2.mean)<-1:nrow(newdf2.mean)
newdf2.max <- subset(newdf2, newdf2$type=='Max.')
rownames(newdf2.max)<-1:nrow(newdf2.max)

#Plot first-rank and second-rank models for NDBI together
dev.new(height=8,width=10,dpi=80,pointsize=12,noRStudioGD = T)
par(mfrow=c(2,1),mar=c(4,4,2,8),mgp=c(2.7,1,0))
plot(newdf2.min$year,newdf2.min$fit,ylim=c(0,1),xlim=c(0,10),las=1,cex=1.5,xaxt='n',ylab='Occurrence probability',xlab='Year',main='',type='l',col="green")
mtext("(a) First-ranked model (interaction)",font=1,side=3,line=0.1,adj=0,cex=1.2)
axis(side=1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels=c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023'))
pg.ci(x="year","newdf2.min",colour=rgb(0,1,0,0.1), lower="lci",upper="uci")

lines(newdf2.mean$year,newdf2.mean$fit,col="black")
pg.ci(x="year","newdf2.mean",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")

lines(newdf2.max$year,newdf2.max$fit,col="red")
pg.ci(x="year","newdf2.max",colour=rgb(1,0,0,0.1), lower="lci",upper="uci")

par(xpd=NA)
legend(x=11,y=1,legend = c("Min.","Mean","Max."),title = "NDBI",col=c("green","black","red"),lty=1)
par(xpd=F)

plot(newdf3$ndbi,newdf3$fit,ylim=c(0,1),las=1,cex=1.5,xaxt='l',ylab='Occurrence probability',xlab='NDBI',main='',type='l')
mtext("(b) Second-ranked model (NDBI only)",font=1,side=3,line=0.1,adj=0,cex=1.2)
pg.ci(x="ndbi","newdf3",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")




# plot ndbi effect at middle year (2017)
ndbi.pr2<-data.frame(year = unique(platystatCatchment$year)[5],ndbi=seq(from=min(platystatCatchment$ndbi,na.rm=T),to=max(platystatCatchment$ndbi,na.rm=T),length.out=50))
ndbi.pr2

ndbi.pr2$predicted_occurrence <- predict(NDBI.int$gam, newdata = ndbi.pr2, type = "response")

# Plot the results
plot(ndbi.pr2$ndbi, ndbi.pr2$predicted_occurrence,
     type = "l", 
     xlab = "NDBI",
     ylab = "Predicted Occurrence",
     main = "Predicted Platypus Occurrence vs. NDBI for Year 2013")




#NDVI CANDIDATE MODELS
NDVI.int <- gamm4(occurrence ~ t2(year, k=10) + t2(ndvi, k=10, bs="fs") + t2(year, ndvi, k=10, bs=c("tp", "fs")), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDVI.int$gam)

NDVI.add <- gamm4(occurrence ~ t2(year, k=10) + t2(ndvi, k=10, bs="fs"), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDVI.add$gam)

NDVI.ndvi <- gamm4(occurrence ~ t2(ndvi, k=10, bs="fs"), random = ~(1|siteID/catchment), data = platystatCatchment, family = binomial)
summary(NDVI.ndvi$gam)

NDVI.cand<-list(NDVI.int$mer,NDVI.add$mer,gamm.year$mer,NDVI.ndvi$mer,gamm.NULL$mer)
NDVI.names<-c("interaction","additive","year only","ndvi only","NULL")
NDVI.tab<-aictab(NDVI.cand,modnames = NDVI.names,second.ord = T,sort = T)
NDVI.tab


### Plot NDVI effect only ###
#plotting ndvi in different years
seq(min(platystatCatchment$ndvi,na.rm=T),max(platystatCatchment$ndvi,na.rm=T),length.out=50)

ndvi.sum<-summary(platystatCatchment$ndvi)[c("Min.","1st Qu.","Mean","Max.")]
newdf4 <- data.frame(year=rep(min(platystatCatchment$year):max(platystatCatchment$year),length(ndvi.sum)))
newdf4$ndvi<-rep(ndvi.sum,rep(length(unique(newdf4$year)),length(ndvi.sum)))
newdf4$type<-rep(names(ndvi.sum),rep(length(unique(newdf4$year)),length(ndvi.sum)))
ndvi.pr2<-predict(NDVI.add$gam,newdata=newdf4,se.fit=T,type='response')
newdf4$fit<-ndvi.pr2$fit
newdf4$se.fit<-ndvi.pr2$se.fit
newdf4$lci <- newdf4$fit-(newdf4$se.fit*1.96)
newdf4$uci <- newdf4$fit+(newdf4$se.fit*1.96)
str(ndvi.pr2)
head(newdf4);dim(newdf4)

#############################
newdf4 <- data.frame(year=rep(min(platystatCatchment$year):max(platystatCatchment$year),length(ndvi.sum)))
newdf4$ndvi<-rep(ndvi.sum,rep(length(unique(newdf4$year)),length(ndvi.sum)))
newdf4$type<-rep(names(ndvi.sum),rep(length(unique(newdf4$year)),length(ndvi.sum)))
newdf4a<-newdf4
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad4<-predict(NDVI.add$gam, newdata = newdf4a, se.fit = T, type="link")
newdf4<-data.frame(newdf4, fit.link=pr_quad4$fit, se.link=pr_quad4$se.fit)
newdf4$lci.link<-newdf4$fit-(newdf4$se*1.96)
newdf4$uci.link<-newdf4$fit+(newdf4$se*1.96)

newdf4$fit<-invlogit(newdf4$fit.link)
newdf4$se<-invlogit(newdf4$se.link)
newdf4$lci<-invlogit(newdf4$lci.link)
newdf4$uci<-invlogit(newdf4$uci.link)
# predict on the link scale (logit scale) 
#########################

# NDVI only model (first-ranked model)
summary(NDVI.ndvi$gam)

newdf5a<-data.frame(ndvi=seq(min(platystatCatchment$ndvi,na.rm=T),max(platystatCatchment$ndvi,na.rm=T),length.out=50))
head(newdf5a);dim(newdf5a)
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad5<-predict(NDVI.ndvi$gam, newdata = newdf5a, se.fit = T, type="link")
newdf5<-data.frame(newdf5a, fit.link=pr_quad5$fit, se.link=pr_quad5$se.fit)
newdf5$lci.link<-newdf5$fit-(newdf5$se*1.96)
newdf5$uci.link<-newdf5$fit+(newdf5$se*1.96)

newdf5$fit<-invlogit(newdf5$fit.link)
newdf5$se<-invlogit(newdf5$se.link)
newdf5$lci<-invlogit(newdf5$lci.link)
newdf5$uci<-invlogit(newdf5$uci.link)
head(newdf4,3);dim(newdf4)
head(newdf5,3);dim(newdf5)

newdf4.min <- subset(newdf4, newdf4$type=='Min.')
rownames(newdf2.min)<-1:nrow(newdf2.min)
newdf4.mean <- subset(newdf4, newdf4$type=='Mean')
rownames(newdf4.mean)<-1:nrow(newdf4.mean)
newdf4.max <- subset(newdf4, newdf4$type=='Max.')
rownames(newdf4.max)<-1:nrow(newdf4.max)

#Plot first-rank and second-rank models for NDBI together
dev.new(height=8,width=10,dpi=80,pointsize=12,noRStudioGD = T)
par(mfrow=c(2,1),mar=c(4,4,2,8),mgp=c(2.7,1,0))

plot(newdf5$ndvi,newdf5$fit,ylim=c(0,1),las=1,cex=1.5,xaxt='l',ylab='Occurrence probability',xlab='NDVI',main='',type='l')
mtext("(a) First-ranked model (NDVI only)",font=1,side=3,line=0.1,adj=0,cex=1.2)
pg.ci(x="ndvi","newdf5",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")

plot(newdf4.min$year,newdf4.min$fit,ylim=c(0,1),xlim=c(0,10),las=1,cex=1.5,xaxt='n',ylab='Occurrence probability',xlab='Year',main='',type='l',col="green")
mtext("(b) Second-ranked model (additive)",font=1,side=3,line=0.1,adj=0,cex=1.2)
axis(side=1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels=c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023'))
pg.ci(x="year","newdf4.min",colour=rgb(0,1,0,0.1), lower="lci",upper="uci")

lines(newdf4.mean$year,newdf4.mean$fit,col="black")
pg.ci(x="year","newdf4.mean",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")

lines(newdf4.max$year,newdf4.max$fit,col="red")
pg.ci(x="year","newdf4.max",colour=rgb(1,0,0,0.1), lower="lci",upper="uci")

par(xpd=NA)
legend(x=11,y=1,legend = c("Min.","Mean","Max."),title = "NDVI",col=c("green","black","red"),lty=1)
par(xpd=F)






#Urban Land Cover (ULC)

head(platystatCatchment);dim(platystatCatchment)
LCdat<-platystatCatchment[-which(is.na(platystatCatchment$LC)),]
LCdat<-LCdat[-which(is.na(LCdat$occurrence)),]
rownames(LCdat)<-1:nrow(LCdat)
head(LCdat);dim(LCdat)

LCdat1<-read.table("00_Data/LCdat.txt",header=T)
LCdat1$siteID<-as.factor(LCdat1$siteID)
LCdat1$catchment<-as.factor(LCdat1$catchment)
LCdat1$year<-LCdat1$year-min(LCdat1$year)
head(LCdat1);dim(LCdat1)
#All NAs have been omitted

# CANDIDATE MODEL SELECTION AGAIN
ULC.year <- gamm4(occurrence ~ year, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.year$gam)

ULC.5mTrf <- gamm4(occurrence ~ X5mTrf, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.5mTrf$gam)

ULC.5mMrf <- gamm4(occurrence ~ X5mMrf, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.5mMrf$gam)

ULC.NULL <- gamm4(occurrence ~ 1, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.NULL$gam)

ULCtemp.cand<-list(ULC.year$mer,ULC.5mTrf$mer,ULC.5mMrf$mer,ULC.NULL$mer)
ULCtemp.names<-c("year interaction","5mTrf interaction","5mMrf interaction","year additive","5mTrf additive","5mMrf additive","year only","ULC only","ULC 5mTrf only","ULC 5mMrf only","NULL")
ULCtemp.tab<-aictab(ULC.cand,modnames = ULC.names,second.ord = T,sort = T)
ULCtemp.tab
# year still the best for the ULC model

#ULC CANDIDATE MODELS

ULC.LC <- gamm4(occurrence ~ LC, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.LC$gam)

ULC.int <- gamm4(occurrence ~ year*LC, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.int$gam)

ULC.add <- gamm4(occurrence ~ year+LC, random = ~(1|siteID/catchment), data = LCdat1, family = binomial)
summary(ULC.add$gam)

ULC.cand<-list(ULC.int$mer,ULC.add$mer,ULC.year$mer,ULC.LC$mer,ULC.NULL$mer)
ULC.names<-c("interaction","additive","year only","ULC-only","NULL")
ULC.tab<-aictab(ULC.cand,modnames = ULC.names,second.ord = T,sort = T)
ULC.tab


#plotting ULC in different years
seq(min(LCdat1$LC,na.rm=T),max(LCdat1$LC,na.rm=T),length.out=50)

ULC.sum<-summary(LCdat1$LC)[c("Min.","1st Qu.","Mean","Max.")]
newdf6 <- data.frame(year=rep(min(LCdat1$year):max(LCdat1$year),length(ULC.sum)))
newdf6$LC<-rep(ULC.sum,rep(length(unique(LCdat1$year)),length(ULC.sum)))
newdf6$type<-rep(names(ULC.sum),rep(length(unique(newdf6$year)),length(ULC.sum)))
ULC.pr<-predict(ULC.int$gam,newdata=newdf6,se.fit=T,type='response')
newdf6$fit<-ULC.pr$fit
newdf6$se.fit<-ULC.pr$se.fit
newdf6$lci <- newdf6$fit-(newdf6$se.fit*1.96)
newdf6$uci <- newdf6$fit+(newdf6$se.fit*1.96)
str(ULC.pr)
head(newdf6);dim(newdf)

#############################
# ULC interactive model (second-rank model)
newdf6 <- data.frame(year=rep(min(LCdat1$year):max(LCdat1$year),length(ULC.sum)))
newdf6$LC<-rep(ULC.sum,rep(length(unique(newdf6$year)),length(ULC.sum)))
newdf6$type<-rep(names(ULC.sum),rep(length(unique(newdf6$year)),length(ULC.sum)))
newdf6a<-newdf6
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad6<-predict(ULC.int$gam, newdata = newdf6a, se.fit = T, type="link")
newdf6<-data.frame(newdf6, fit.link=pr_quad6$fit, se.link=pr_quad6$se.fit)
newdf6$lci.link<-newdf6$fit-(newdf6$se*1.96)
newdf6$uci.link<-newdf6$fit+(newdf6$se*1.96)

newdf6$fit<-invlogit(newdf6$fit.link)
newdf6$se<-invlogit(newdf6$se.link)
newdf6$lci<-invlogit(newdf6$lci.link)
newdf6$uci<-invlogit(newdf6$uci.link)
# predict on the link scale (logit scale) 
#########################

# ULC only model (first-rank model)
summary(ULC.LC$gam)

newdf7a<-data.frame(LC=seq(min(LCdat1$LC,na.rm=T),max(LCdat1$LC,na.rm=T),length.out=50))
head(newdf7a);dim(newdf7a)
# Estimates 
# if fitting a binomial, CIs shouldn't go below zero. So instead of type="response", use type="link" and back-transform after you make the predictions, using invlogit in the arm package
pr_quad7<-predict(ULC.LC$gam, newdata = newdf7a, se.fit = T, type="link")
newdf7<-data.frame(newdf7a, fit.link=pr_quad7$fit, se.link=pr_quad7$se.fit)
newdf7$lci.link<-newdf7$fit-(newdf7$se*1.96)
newdf7$uci.link<-newdf7$fit+(newdf7$se*1.96)

newdf7$fit<-invlogit(newdf7$fit.link)
newdf7$se<-invlogit(newdf7$se.link)
newdf7$lci<-invlogit(newdf7$lci.link)
newdf7$uci<-invlogit(newdf7$uci.link)
head(newdf6,3);dim(newdf6)
head(newdf7,3);dim(newdf7)
newdf6.min <- subset(newdf6, newdf6$type=='Min.')
rownames(newdf6.min)<-1:nrow(newdf6.min)
newdf6.mean <- subset(newdf6, newdf6$type=='Mean')
rownames(newdf6.mean)<-1:nrow(newdf6.mean)
newdf6.max <- subset(newdf6, newdf6$type=='Max.')
rownames(newdf6.max)<-1:nrow(newdf6.max)

#Plot first-rank and second-rank models for ULC together
dev.new(height=8,width=10,dpi=80,pointsize=12,noRStudioGD = T)
par(mfrow=c(2,1),mar=c(4,4,2,10),mgp=c(2.7,1,0))
plot(newdf6.min$year,newdf6.min$fit,ylim=c(0,1),xlim=c(0,10),las=1,cex=1.5,xaxt='n',ylab='Occurrence probability',xlab='Year',main='',type='l',col="green")
mtext("(a) First-ranked model (interaction)",font=1,side=3,line=0.1,adj=0,cex=1.2)
axis(side=1,at=c(0,7,10),labels=c('2013','2019','2023'))
pg.ci(x="year","newdf6.min",colour=rgb(0,1,0,0.1), lower="lci",upper="uci")

lines(newdf6.mean$year,newdf6.mean$fit,col="black")
pg.ci(x="year","newdf6.mean",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")

lines(newdf6.max$year,newdf6.max$fit,col="red")
pg.ci(x="year","newdf6.max",colour=rgb(1,0,0,0.1), lower="lci",upper="uci")

par(xpd=NA)
legend(x=11,y=1,legend = c("Min.","Mean","Max."),title = "Urban land cover %",col=c("green","black","red"),lty=1)
par(xpd=F)

plot(newdf7$LC,newdf7$fit,ylim=c(0,1),las=1,cex=1.5,xaxt='l',ylab='Occurrence probability',xlab='Urban land cover %',main='',type='l')
mtext("(b) Second-ranked model (urban land cover only)",font=1,side=3,line=0.1,adj=0,cex=1.2)
pg.ci(x="LC","newdf7",colour=rgb(0,0,0,0.1), lower="lci",upper="uci")



#Candidate models ALL
ALL.cand<-list(NDVI.int$mer,NDVI.intb$mer,NDVI.intc$mer,NDVI.add$mer,NDVI.addb$mer,NDVI.addc$mer,gamm.year$mer,NDVI.ndvi$mer,NDBI.int$mer,NDBI.intb$mer,NDBI.intc$mer,NDBI.add$mer,NDBI.addb$mer,NDBI.addc$mer,NDBI.ndbi$mer,gamm.5mTrf$mer,gamm.5mMrf$mer,gamm.NULL$mer)
ALL.names<-c("NDVI*year","5mTrf*NDVI","5mMrf*NDVI","year+NDVI","5mTrf+NDVI","5mMrf+NDVI","year only","NDVI only","NDBI*year","5mTrf*NDBI","5mMrf*NDBI","year+NDBI","5mTrf+NDBI","5mMrf+NDBI","NDBI only","5mTrf only","5mMrf only","NULL")
ALL.tab<-aictab(ALL.cand,modnames = ALL.names,second.ord = T,sort = T)
ALL.tab


# Graph year + ndvi model

summary(gammmod2$gam)
plot(gammmod2$gam,pages=1)


# Graph year + ndbi model
summary(gammmod3$gam)
plot(gammmod3$gam,pages=1)

mean(platystatCatchment$ndbi,na.rm=T)
newdf3 <- data.frame(ndbi=rep(seq(min(platystatCatchment$ndbi,na.rm=T),max(platystatCatchment$ndbi,na.rm=T),length.out=50),length(unique(platystatCatchment$year))))
newdf3$year <- c(rep(unique(platystatCatchment$year),rep(50,length(unique(platystatCatchment$year)))))
head(newdf3);dim(newdf3)

mod3_pr<-predict(gammmod3$gam,newdata= newdf3,se.fit=T,type='response')
newdf3$fit<-mod3_pr$fit
newdf3$se.fit<-mod3_pr$se.fit
newdf3$lci <- newdf3$fit-(newdf3$se.fit*1.96)
newdf3$uci <- newdf3$fit+(newdf3$se.fit*1.96)
str(mod3_pr)

plot(newdf3$ndbi,newdf3$fit,ylim=c(min(newdf3$lci), max(newdf3$uci)),las=1,cex=1.5,xaxt='n',ylab='occurrence',xlab='NDBI',type='l')
arrows(newdf3$ndbi,newdf3$lci,newdf3$year,newdf3$uci,length=0.1,angle=90,code=3)









## Fit GAMs ##

library(gratia)

gammod.a <- gam(occurrence~year,random=~(1|siteID/catchment), data=platystatCatchment,method="REML")
summary(gammod.a)

gammod1 <- gam(occurrence ~ s(year,k=10), random=~(1|siteID/catchment), data=platystatCatchment, family=binomial,method="REML")
summary(gammod1)

gammod2 <- gam(occurrence ~ s(year,k=10) + s(ndvi,k=10), random=~(1|siteID/catchment), data=platystatCatchment, family=binomial,method="REML")
summary(gammod2)

gammod3 <- gam(occurrence ~ s(year,k=10) + s(ndbi,k=10), random=~(1|siteID/catchment), data=platystatCatchment, family=binomial,method="REML")
summary(gammod3)

gammod4 <- gam(occurrence~s(year,k=3)+s(LC,k=3), random=~(1|siteID/catchment), data=platystatCatchment, family=binomial,method="REML")
summary(gammod4)

smooth_estimates(gammod1)
gammod.a|> draw(grouped_by = TRUE)
gammod1 |> draw(grouped_by = TRUE)
gammod2 |> draw(grouped_by = TRUE)
gammod3 |> draw(grouped_by = TRUE)
gammod4 |> draw(grouped_by = TRUE)

# Different AIC method? Or different comparison method
AIC(gammod.a, gammod1,gammod2,gammod3,gammod4)




