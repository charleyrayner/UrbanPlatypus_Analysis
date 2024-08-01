install.packages("lme4")
library(lme4)
library(AICcmodavg)
#fix up data
#download package mgcv (generalised addditive mixed model)
install.packages("mgcv")
library(mgcv)
dir()
dir("UQ Honours")
dir("GIS2024")

platystatCatchment<-read.table("platystatcatchment.txt",header=T)

platystatCatchment$siteID<-as.factor(platystatCatchment$siteID)
platystatCatchment$catchment<-as.factor(platystatCatchment$catchment)

platystatCatchment$year<-platystatCatchment$year-min(platystatCatchment$year)
head(platystatCatchment);dim(platystatCatchment)

unique(platystatCatchment$siteID)
unique(platystatCatchment$year)
str(platystatCatchment)

#model using glmer with year and ndvi as linear terms
B1<-glmer(occurrence~year*ndvi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B1)

#model using glmer with year as a linear term and ndvi as an additive effect
B2<-glmer(occurrence~year+ndvi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B2)

#model using glmer with year and ndbi as linear terms
B3<-glmer(occurrence~year*ndbi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B3)

#model using glmer with year as a linear term and ndbi as an additive effect
B4<-glmer(occurrence~year+ndbi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B4)

#model using glmer with year as a linear term
M5<-glmer(occurrence~year+(1|catchment/siteID),family=binomial,data=platystat)
summary(M5)

#model using glmer with year and LC as linear terms
M6<-glmer(occurrence~year*LC+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(M6)

#model using glmer with year as a linear term and LC as an additive effect
M7<-glmer(occurrence~year+LC+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(M7)

#null model
Mnull<-glmer(occurrence~1+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(Mnull)


AICc(B1);AICc(B2);AICc(B3);AICc(B4);AICc(M5);AICc(M6);AICc(M7);AICc(Mnull)

?aictab
set1<-list("year*ndvi"=B1, "year+ndvi"=B2,"year*ndbi"=B3,"year+ndbi"=B4,"year"=M5,"year*LC"=M6,"year+LC"=M7,"null"=Mnull)
tab1<-aictab(cand.set=set1,second.ord = T,sort=T)
tab1


#plot year effect at mean ndvi (from model B2)
year.pr<-data.frame(year = unique(platystatCatchment$year),ndvi=mean(platystatCatchment$ndvi,na.rm=T))
year.pr

#plot year effect at mean ndbi (from model B4)
year.pr2<-data.frame(year = unique(platystatCatchment$year),ndbi=mean(platystatCatchment$ndvi,na.rm=T))
year.pr2

#plot year effect at mean ndbi (from model M6)
year.pr3<-data.frame(year = unique(platystatCatchment$year),LC=mean(platystatCatchment$LC,na.rm=T))
year.pr3

#plot ndvi effect at middle year (2017) (from model B2)
ndvi.pr2<-data.frame(year = unique(platystatCatchment$year)[5],ndvi=seq(from=min(platystatCatchment$ndvi,na.rm=T),to=max(platystatCatchment$ndvi,na.rm=T),length.out=50))
ndvi.pr2

#plot ndbi effect at middle year (from model B4)
ndbi.pr2<-data.frame(year = unique(platystatCatchment$year)[5],ndbi=seq(from=min(platystatCatchment$ndbi,na.rm=T),to=max(platystatCatchment$ndbi,na.rm=T),length.out=50))
ndbi.pr2

#plot LC effect at middle year (from model M6)
LC.pr2<-data.frame(year = unique(platystatCatchment$year)[5],LC=seq(from=min(platystatCatchment$LC,na.rm=T),to=max(platystatCatchment$LC,na.rm=T),length.out=50))
LC.pr2

#plot ndvi effect at all years (from model B1)
ndvi.pr3<-data.frame(year = rep(unique(platystatCatchment$year),rep(50,length(unique(platystatCatchment$year)))),ndvi=seq(from=min(platystatCatchment$ndvi,na.rm=T),to=max(platystatCatchment$ndvi,na.rm=T),length.out=50))
ndvi.pr3

#plot ndbi effect at all years (from model B3)
ndbi.pr3<-data.frame(year = rep(unique(platystatCatchment$year),rep(50,length(unique(platystatCatchment$year)))),ndbi=seq(from=min(platystatCatchment$ndbi,na.rm=T),to=max(platystatCatchment$ndbi,na.rm=T),length.out=50))
ndbi.pr3

#plot LC effect at all years (from model M7)
LC.pr3<-data.frame(year = rep(unique(platystatCatchment$year),rep(50,length(unique(platystatCatchment$year)))),LC=seq(from=min(platystatCatchment$LC,na.rm=T),to=max(platystatCatchment$LC,na.rm=T),length.out=50))
LC.pr3

#predict ndvi effects from B1
B1.pr<-predictSE(mod = B1, newdata = ndvi.pr3,type = "response", se.fit = T)
B1.pr2<-data.frame(ndvi.pr3)
B1.pr2$fit<-B1.pr$fit
B1.pr2$se<-B1.pr$se
B1.pr2$lci<-B1.pr$fit-(B1.pr$se*1.96)
B1.pr2$uci<-B1.pr$fit+(B1.pr$se*1.96)
head(B1.pr2)

#predict ndbi effects from B3
B3.pr<-predictSE(mod = B3, newdata = ndbi.pr3,type = "response", se.fit = T)
B3.pr2<-data.frame(ndbi.pr3)
B3.pr2$fit<-B3.pr$fit
B3.pr2$se<-B3.pr$se
B3.pr2$lci<-B3.pr$fit-(B3.pr$se*1.96)
B3.pr2$uci<-B3.pr$fit+(B3.pr$se*1.96)
head(B3.pr2)

#predict LC effects from M7
L3.pr<-predictSE(mod = M7, newdata = LC.pr3,type = "response", se.fit = T)
L3.pr2<-data.frame(LC.pr3)
L3.pr2$fit<-L3.pr$fit
L3.pr2$se<-L3.pr$se
L3.pr2$lci<-L3.pr$fit-(L3.pr$se*1.96)
L3.pr2$uci<-L3.pr$fit+(L3.pr$se*1.96)
head(L3.pr2)

#predict year effects on ndvi from B2
B2.pr<-predictSE(mod = B2, newdata = year.pr,type = "response", se.fit = T)
B2.pr2<-data.frame(year.pr)
B2.pr2$fit<-B2.pr$fit
B2.pr2$se<-B2.pr$se
B2.pr2$lci<-B2.pr2$fit-(B2.pr2$se*1.96)
B2.pr2$uci<-B2.pr2$fit+(B2.pr2$se*1.96)

#predict year effects on ndbi from B4
B4.pr<-predictSE(mod = B4, newdata = year.pr2,type = "response", se.fit = T)
B4.pr2<-data.frame(year.pr2)
B4.pr2$fit<-B4.pr$fit
B4.pr2$se<-B4.pr$se
B4.pr2$lci<-B4.pr2$fit-(B4.pr2$se*1.96)
B4.pr2$uci<-B4.pr2$fit+(B4.pr2$se*1.96)

#predict year effects on LC from M6
L4.pr<-predictSE(mod = M6, newdata = year.pr3,type = "response", se.fit = T)
L4.pr2<-data.frame(year.pr3)
L4.pr2$fit<-L4.pr$fit
L4.pr2$se<-L4.pr$se
L4.pr2$lci<-L4.pr2$fit-(L4.pr2$se*1.96)
L4.pr2$uci<-L4.pr2$fit+(L4.pr2$se*1.96)

#predict ndvi effects from B2
B2.pr.ndvi<-predictSE(mod = B2, newdata = ndvi.pr,type = "response", se.fit = T)
B2.pr.ndvi2<-data.frame(ndvi.pr)
B2.pr.ndvi2$fit<-B2.pr.ndvi$fit
B2.pr.ndvi2$se<-B2.pr.ndvi$se
B2.pr.ndvi2$lci<-B2.pr.ndvi$fit-(B2.pr.ndvi$se*1.96)
B2.pr.ndvi2$uci<-B2.pr.ndvi$fit+(B2.pr.ndvi$se*1.96)
head(B2.pr.ndvi2)

#predict ndbi effects from B4
B4.pr.ndbi<-predictSE(mod = B4, newdata = ndbi.pr,type = "response", se.fit = T)
B4.pr.ndbi2<-data.frame(ndbi.pr)
B4.pr.ndbi2$fit<-B4.pr.ndbi$fit
B4.pr.ndbi2$se<-B4.pr.ndvi$se
B4.pr.ndbi2$lci<-B4.pr.ndvi$fit-(B4.pr.ndvi$se*1.96)
B4.pr.ndbi2$uci<-B4.pr.ndvi$fit+(B4.pr.ndvi$se*1.96)
head(B4.pr.ndbi2)

#predict ndbi effects from M6
L4.pr.ndbi<-predictSE(mod = B4, newdata = ndbi.pr,type = "response", se.fit = T)
L4.pr.LC<-data.frame(L4.pr)
L4.pr.LC2$fit<-L4.pr.LC$fit
L4.pr.LC2$se<-L4.pr.LC$se
L4.pr.LC2$lci<-L4.pr.LC$fit-(L4.pr.LC$se*1.96)
L4.pr.LC2$uci<-L4.pr.LC$fit+(L4.pr.LC$se*1.96)
head(L4.pr.LC2)

### NOT WORKING ###
#plot ndvi effect at middle year
ndvi.pr4<-data.frame(year = unique(platystatCatchment$year)[9],ndvi=seq(from=min(platystatCatchment$ndvi),to=max(platystatCatchment$ndvi),length.out=50))
ndvi.pr4

#plot ndbi effect at middle year
ndbi.pr4<-data.frame(year = unique(platystatCatchment$year)[5],ndbi=seq(from=min(platystatCatchment$ndbi),to=max(platystatCatchment$ndbi),length.out=50))
ndbi.pr4

dev.new(width=8, height=4, dpi=80, pointsize=14, noRStudioGD = T)
par(mfrow=c(1,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))

# relationship between year and probability of occurrence
plot(B2.pr2$year,B2.pr2$fit, pch=20, xaxt="n",ylim= c(0,1),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(B2.pr2$year,B2.pr2$lci,lty=2)
lines(B2.pr2$year,B2.pr2$uci,lty=2)
axis(1,at=B2.pr2$year, cex.axis=0.7,labels=unique(platystatCatchment$year),las=2)
title(mgp=c(2.3,0.8,0),xlab="Year")
mtext(text="(a)", side = 3, line = 0.5, adj = 0, cex = 1)

# relationship between mean ndvi and probability of occurrence
head(B2.pr.ndvi2);dim(B2.pr.ndvi2)
plot(B2.pr.ndvi2$ndvi,B2.pr.ndvi2$fit, pch=20,ylim= c(0,1),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(B2.pr.ndvi2$ndvi,B2.pr.ndvi2$lci,lty=2)
lines(B2.pr.ndvi2$ndvi,B2.pr.ndvi2$uci,lty=2)
#axis(1,at=B2.pr.ndvi2$ndvi, cex.axis=0.7)
title(mgp=c(2.3,0.8,0),xlab="Mean ndvi")
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)

# relationship between mean ndbi and probability of occurrence
head(B4.pr.ndbi2);dim(B4.pr.ndbi2)
plot(B4.pr.ndbi2$ndvi,B4.pr.ndbi2$fit, pch=20,ylim= c(-1,1),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(B4.pr.ndbi2$ndbi,B4.pr.ndbi2$lci,lty=2)
lines(B2.pr.ndbi2$ndbi,B4.pr.ndbi2$uci,lty=2)
#axis(1,at=B2.pr.ndbi2$ndbi, cex.axis=0.7)
title(mgp=c(2.3,0.8,0),xlab="Mean ndbi")
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)

# relationship between urban LC and probability of occurrence
head(L4.pr.LC2);dim(L4.pr.LC2)
plot(L4.pr.LC2$LC,L4.pr.LC2$fit, pch=20,ylim= c(0,100),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(L4.pr.LC2$LC,L4.pr.LC2$lci,lty=2)
lines(L4.pr.LC2$LC,L4.pr.LC2$uci,lty=2)
#axis(1,at=B2.pr.ndbi2$ndbi, cex.axis=0.7)
title(mgp=c(2.3,0.8,0),xlab="LC")
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)

dev.new(width=7, height=12, dpi=70, pointsize=14, noRStudioGD = T)
par(mfrow=c(5,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))


head(B1.pr2)
head(B3.pr2)
head(L4.pr2)

# all years for ndvi (Model 1)
years<-unique(B1.pr2$year)
years.name<-years+2013

#NDVI
for(i in 1:length(years)){
  year.thisrun1<-years[i]
  dat.thisrun1<-B1.pr2[B1.pr2$year==year.thisrun1,]
  plot(dat.thisrun1$ndvi,dat.thisrun1$fit, pch=20,ylim= c(0,1),ylab="",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l",main=years.name[i])
  title(ylab="Occurrence Prob",cex.lab=1)
  lines(dat.thisrun1$ndvi,dat.thisrun1$lci,lty=2)
  lines(dat.thisrun1$ndvi,dat.thisrun1$uci,lty=2)
  #axis(1,at=dat.thisrun1$ndvi, cex.axis=0.7,labels=unique(platystatCatchment$year),las=2)
  title(mgp=c(2.3,0.8,0),xlab="ndvi")
  mtext(text=paste("(",letters[i],")",sep=""), side = 3, line = 0.5, adj = 0, cex = 1)
}


#NDBI
for(i in 1:length(years)){
  year.thisrun2<-years[i]
  dat.thisrun2<-B3.pr2[B3.pr2$year==year.thisrun2,]
  plot(dat.thisrun2$ndbi,dat.thisrun2$fit, pch=20,ylim= c(0,1),ylab="",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l",main=years.name[i])
  title(ylab="Occurrence Prob",cex.lab=1)
  lines(dat.thisrun2$ndbi,dat.thisrun2$lci,lty=2)
  lines(dat.thisrun2$ndbi,dat.thisrun2$uci,lty=2)
  #axis(1,at=dat.thisrun1$ndvi, cex.axis=0.7,labels=unique(platystatCatchment$year),las=2)
  title(mgp=c(2.3,0.8,0),xlab="mean NDBI")
  mtext(text=paste("(",letters[i],")",sep=""), side = 3, line = 0.5, adj = 0, cex = 1)
}


# Urban LC
for(i in 1:length(years)){
  year.thisrun3<-years[i]
  dat.thisrun3<-L4.pr2[L4.pr2$year==year.thisrun3,]
  plot(dat.thisrun3$LC,dat.thisrun3$fit, pch=20,ylim= c(0,1),xlim=c(0,80),ylab="",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l",main=years.name[i])
  title(ylab="Occurrence Prob",cex.lab=1)
  lines(dat.thisrun3$LC,dat.thisrun3$lci,lty=2)
  lines(dat.thisrun3$LC,dat.thisrun3$uci,lty=2)
  #axis(1,at=dat.thisrun1$ndvi, cex.axis=0.7,labels=unique(platystatCatchment$year),las=2)
  title(mgp=c(2.3,0.8,0),xlab="urban LC %")
  mtext(text=paste("(",letters[i],")",sep=""), side = 3, line = 0.5, adj = 0, cex = 1)
}
?plot

#editing plots
?par

#study locations, AIC results, and above plots


# Plot ndvi
plot(c(1:3),B2.pr2$fit, xlim=c(0.5,3.5), pch=20, xaxt="n",ylim= c((min(m7_c.pr2$lci)),max(B2.pr2$uci)),ylab="Richness (lowest 25%)",xlab="", las = 1, cex = 2.5)
arrows(c(1:3),B2.pr2$lci,c(1:3),B2.pr2$uci,length=0.03,code=3,angle=90)
axis(1,at=c(1:3),labels=F)
axis(1,at=c(0.8,2,3.2),labels=B2.pr2$year,tick=F, cex.axis=1)
title(mgp=c(2.3,0.8,0),xlab="year")
B3.tab2
text(2.1,max(B3.pr2$uci),as.expression(bquote(Delta~"AICc ="~.(paste(round(B3.tab2$Delta_AICc[B3.tab2$Modnames=="ndvi"]-B3.tab2$Delta_AICc[B3.tab2$Modnames=="null"],2),sep="")))),adj=0,col="red", cex=0.9)
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)


#maps = plot of mean ndvi values over time, second plot of variation in ndvi values
summary(platystatCatchment$ndvi,platystatCatchment$year)
head(platystatCatchment);dim(platystatCatchment)
plot(platystatCatchment$year,platystatCatchment$ndvi)
?par

#For appendix
dev.new(width=8, height=4, dpi=80, pointsize=14, noRStudioGD = T)
par(mfrow=c(1,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))

# Graph for change in mean NDVI over time
plot(platystatCatchment$year,platystatCatchment$ndvi, pch=20, ylab="Mean ndvi",xlab="Year", las = 1,cex.axis=0.75,cex.lab=0.75,col=factor(platystatCatchment$catchment))
colors <- c("purple2",
            "deepskyblue2",
            "gold2",
            "deeppink1",
            "green2")
legend("topleft",
       legend = levels(factor(platystatCatchment$catchment)),
       pch = 20, bg="white",
       col = factor(levels(factor(platystatCatchment$catchment))))
mtext(text="(a)", side = 3, line = 0.5, adj = 0, cex = 1)

# Graph for change in LC over time
plot(platystatCatchment$year,platystatCatchment$LC, pch=20, ylab="Urban LC",xlab="Year", las = 1,cex.axis=0.75,cex.lab=0.75,col=factor(platystatCatchment$catchment))
colors <- c("purple2",
            "deepskyblue2",
            "gold2",
            "deeppink1",
            "green2")
legend("topleft",
       legend = levels(factor(platystatCatchment$catchment)),
       pch = 20, bg="white",
       col = factor(levels(factor(platystatCatchment$catchment))))
mtext(text="(a)", side = 3, line = 0.5, adj = 0, cex = 1)


plot(platystatCatchment$year,platystatCatchment$stdev, pch=20, ylab="ndvi stdev",xlab="Year", las = 1,cex.axis=0.75,cex.lab=0.75,col=factor(platystatCatchment$catchment))
colors <- c("purple3",
            "deepskyblue3",
            "gold1",
            "deeppink",
            "green3")
# Legend
legend("topleft",
       legend = c("Albert", "Coomera","Nerang","Mudgeeraba","Currumbin"),
       pch = 20,
       col = colors)
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)


#Calcuate proportion of positive platypus sightings over time
T4<-table(platystatCatchment$year,platystatCatchment$occurrence)
T5<-T4[,2]/(T4[,1]+T4[,2])
T6<-data.frame(year=names(T5),platy=T5)
plot(T6$year,T6$platy)





anova(gammmod1$lme)
anova(gammmod1$lme,gammmod1.a$gam)
str(gammmod1$lme)
gammmod1$lme$coefficients
gammmod1$gam$coefficients
anova(gammmod1,gammmod1.a)

library(AICcmodavg)
AICc(gammmod1$gam)
AICc(gammmod1.a)

#Graph model

newdf <- data.frame(year=seq(0,9,length.out=100))
mod1_pr<-predict(gammmod1$gam,newdata=newdf,se.fit=T,type='response')
newdf$fit<-mod1_pr$fit
newdf$se.fit<-mod1_pr$se.fit
newdf$lci <- newdf$fit-(newdf$se.fit*1.96)
newdf$uci <- newdf$fit+(newdf$se.fit*1.96)
str(mod1_pr)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$year,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),las=1,cex=1.5,xaxt='n',ylab='occurrence',xlab='Year')
arrows(newdf$year,newdf$lci,newdf$year,newdf$uci,length=0.1,angle=90,code=3)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$year,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),xlim=c(0,10),las=1,cex=1.5,xaxt='n',ylab='Occurrence prob',xlab='Year',main='Platypus',type='l')
axis(side=1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels=c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023'))
lines(newdf$year,newdf$uci,lty=2)
lines(newdf$year,newdf$lci,lty=2)

#Native vs exotic

nat_mod <- gamm(native~s(Yr, k=5), random=list(Reserve=~1), data=fire_abun5, family=poisson)
summary(nat_mod)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(nat_mod$gam,pages=1)

exo_mod <- gamm(exotic~s(Yr, k=5), random=list(Reserve=~1), data=fire_abun5, family=poisson)
summary(exo_mod)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(exo_mod$gam,pages=1)

#Run a second gamm that does not smooth Yr. This can be done, but I am having issues viewing this output.
gammmod2 <- gamm(all~Yr+factor(Reserve), random=list(Reserve=~1), data=fire_abun5, family=poisson)
summary(gammmod2)

gammmod2$gam
gammmod2$lme

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(fire_abun5$Yr, fire_abun5$all, xlab = "Yr", ylab = "Response variable", main = "All")

plot(gammmod2$lme,page=1)

#nb - gam smooth effect with interations is te()
#Run two gamms, one smoothing year and one that does not?
mod3 <- glmer(occurrence~year+(1|catchment/siteID), data=platystatCatchment, family='binomial')
summary(mod3)

