
install.packages("lme4")
library(lme4)
library(AICcmodavg)
#fix up data
#download package mgcv (generalised addditive mixed model)
install.packages("mgcv")
library(mgcv)
#gamm (function)
dir()
dir("data")

platystat<-read.table("platystat.txt",header=T)
platystatCatchment<-read.table("platystatcatchment.txt",header=T)

#200mBuffer
platystat$siteID<-as.factor(platystat$siteID)
platystat$catchment<-as.factor(platystat$catchment)
#Catchment
platystatCatchment$siteID<-as.factor(platystatCatchment$siteID)
platystatCatchment$catchment<-as.factor(platystatCatchment$catchment)

#200mBuffer
platystat$year<-platystat$year-min(platystat$year)
head(platystat);dim(platystat)
#Catchment
platystatCatchment$year<-platystatCatchment$year-min(platystatCatchment$year)
head(platystatCatchment);dim(platystatCatchment)

#200mBuffer
unique(platystat$siteID)
unique(platystat$year)
str(platystat)
#Catchment
unique(platystatCatchment$siteID)
unique(platystatCatchment$year)
str(platystatCatchment)

#model using glmer with year and ndvi as linear terms

M1<-glmer(occurrence~year*ndvi+(1|catchment/siteID),family=binomial,data=platystat)
summary(M1)

B1<-glmer(occurrence~year*ndvi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B1)


#model using glmer with year as a linear term and ndvi as an additive effect

M2<-glmer(occurrence~year+ndvi+(1|catchment/siteID),family=binomial,data=platystat)
summary(M2)

B2<-glmer(occurrence~year+ndvi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B2)


#model using glmer with year and ndvi as linear terms

M3<-glmer(occurrence~year*ndbi+(1|catchment/siteID),family=binomial,data=platystat)
summary(M3)

B3<-glmer(occurrence~year*ndbi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B3)


#model using glmer with year as a linear term and [variable] as an additive effect

M4<-glmer(occurrence~year+ndbi+(1|catchment/siteID),family=binomial,data=platystat)
summary(M4)

B4<-glmer(occurrence~year+ndbi+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(B4)

#model using glmer with year as a linear term

M5<-glmer(occurrence~year+(1|catchment/siteID),family=binomial,data=platystat)
summary(M5)

#null model

MnullBuffer<-glmer(occurrence~1+(1|catchment/siteID),family=binomial,data=platystat)
summary(MnullBuffer)

MnullCatchment<-glmer(occurrence~1+(1|catchment/siteID),family=binomial,data=platystatCatchment)
summary(MnullCatchment)


AICc(M1);AICc(B1);AICc(M2);AICc(B2);AICc(M3);AICc(B3);AICc(M4);AICc(B4);AICc(M5);AICc(MnullBuffer);AICc(MnullCatchment)

?aictab
set1<-list("year*ndvi_buffer"=M1,"year*ndvi_catchment"=B1, "year+ndvi_buffer"=M2,"year+ndvi_catchment"=B2,"year*ndbi_buffer"=M3,"year*ndbi_catchment"=B3,"year+ndbi_buffer"=M4,"year+ndbi_catchment"=B4,"year"=M5,"null_buffer"=MnullBuffer,"null_catchment"=MnullCatchment)
tab1<-aictab(cand.set=set1,second.ord = T,sort=T)
tab1


#BUFFER
#plot year effect at mean ndvi (from model M2)
year.pr<-data.frame(year = unique(platystat$year),ndvi=mean(platystat$ndvi,na.rm=T))
year.pr

#plot ndvi effect at middle year (2017) (from model M2)
ndvi.pr<-data.frame(year = unique(platystat$year)[9],ndvi=seq(from=min(platystat$ndvi,na.rm=T),to=max(platystat$ndvi,na.rm=T),length.out=50))
ndvi.pr


#plot ndvi effect at all years (from model M1)
ndvi.pr1<-data.frame(year = rep(unique(platystat$year),rep(50,length(unique(platystat$year)))),ndvi=seq(from=min(platystat$ndvi,na.rm=T),to=max(platystat$ndvi,na.rm=T),length.out=50))
ndvi.pr1

#predict ndvi effects from M1
M1.pr<-predictSE(mod = M1, newdata = ndvi.pr1,type = "response", se.fit = T)
M1.pr2<-data.frame(ndvi.pr1)
M1.pr2$fit<-M1.pr$fit
M1.pr2$se<-M1.pr$se
M1.pr2$lci<-M1.pr$fit-(M1.pr$se*1.96)
M1.pr2$uci<-M1.pr$fit+(M1.pr$se*1.96)
head(M1.pr2)

#predict year effects from M2
M2.pr<-predictSE(mod = M2, newdata = year.pr,type = "response", se.fit = T)
M2.pr2<-data.frame(year.pr)
M2.pr2$fit<-M2.pr$fit
M2.pr2$se<-M2.pr$se
M2.pr2$lci<-M2.pr2$fit-(M2.pr2$se*1.96)
M2.pr2$uci<-M2.pr2$fit+(M2.pr2$se*1.96)

#predict ndvi effects from M2
M2.pr.ndvi<-predictSE(mod = M2, newdata = ndvi.pr,type = "response", se.fit = T)
M2.pr.ndvi2<-data.frame(ndvi.pr)
M2.pr.ndvi2$fit<-M2.pr.ndvi$fit
M2.pr.ndvi2$se<-M2.pr.ndvi$se
M2.pr.ndvi2$lci<-M2.pr.ndvi$fit-(M2.pr.ndvi$se*1.96)
M2.pr.ndvi2$uci<-M2.pr.ndvi$fit+(M2.pr.ndvi$se*1.96)
head(M2.pr.ndvi2)


#plot ndvi effect at middle year
ndvi.pr<-data.frame(year = unique(platystat$year)[9],ndvi=seq(from=min(platystat$ndvi),to=max(platystat$ndvi),length.out=50))
ndvi.pr

#plot ndbi effect at middle year
ndbi.pr<-data.frame(year = unique(platystat$year)[9],ndbi=seq(from=min(platystat$ndbi),to=max(platystat$ndbi),length.out=50))
ndbi.pr

dev.new(width=8, height=4, dpi=80, pointsize=14, noRStudioGD = T)
par(mfrow=c(1,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))

# relationship between year and probability of occurrence
plot(M2.pr2$year,M2.pr2$fit, pch=20, xaxt="n",ylim= c(0,1),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(M2.pr2$year,M2.pr2$lci,lty=2)
lines(M2.pr2$year,M2.pr2$uci,lty=2)
axis(1,at=M2.pr2$year, cex.axis=0.7,labels=unique(platystat$year),las=2)
title(mgp=c(2.3,0.8,0),xlab="Year")
mtext(text="(a)", side = 3, line = 0.5, adj = 0, cex = 1)

# relationship between mean ndvi and probability of occurrence
head(M2.pr.ndvi2);dim(M2.pr.ndvi2)
plot(M2.pr.ndvi2$ndvi,M2.pr.ndvi2$fit, pch=20,ylim= c(0,1),ylab="Probability of occurrence",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l")
lines(M2.pr.ndvi2$ndvi,M2.pr.ndvi2$lci,lty=2)
lines(M2.pr.ndvi2$ndvi,M2.pr.ndvi2$uci,lty=2)
#axis(1,at=M2.pr.ndvi2$ndvi, cex.axis=0.7)
title(mgp=c(2.3,0.8,0),xlab="Mean ndvi")
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)



dev.new(width=7, height=12, dpi=70, pointsize=14, noRStudioGD = T)
par(mfrow=c(5,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))


head(M1.pr2)

# all years for ndvi (Model 1)
years<-unique(M1.pr2$year)
years.name<-years+2013


#NDVI
for(i in 1:length(years)){
  year.thisrun<-years[i]
  dat.thisrun<-M1.pr2[M1.pr2$year==year.thisrun,]
  plot(dat.thisrun$ndvi,dat.thisrun$fit, pch=20,ylim= c(0,1),ylab="",xlab="", las = 1, cex = 2.5,lwd=2,col="red",type="l",main=years.name[i])
  title(ylab="Occurrence Prob",cex.lab=1)
  lines(dat.thisrun$ndvi,dat.thisrun$lci,lty=2)
  lines(dat.thisrun$ndvi,dat.thisrun$uci,lty=2)
  #axis(1,at=dat.thisrun$ndvi, cex.axis=0.7,labels=unique(platystat$year),las=2)
  title(mgp=c(2.3,0.8,0),xlab="ndvi")
  mtext(text=paste("(",letters[i],")",sep=""), side = 3, line = 0.5, adj = 0, cex = 1)
}



#editing plots
?par

#study locations, AIC results, and above plots


# Plot ndvi
plot(c(1:3),M2.pr2$fit, xlim=c(0.5,3.5), pch=20, xaxt="n",ylim= c((min(m7_c.pr2$lci)),max(M2.pr2$uci)),ylab="Richness (lowest 25%)",xlab="", las = 1, cex = 2.5)
arrows(c(1:3),M2.pr2$lci,c(1:3),M2.pr2$uci,length=0.03,code=3,angle=90)
axis(1,at=c(1:3),labels=F)
axis(1,at=c(0.8,2,3.2),labels=M2.pr2$year,tick=F, cex.axis=1)
title(mgp=c(2.3,0.8,0),xlab="year")
M3.tab2
text(2.1,max(M3.pr2$uci),as.expression(bquote(Delta~"AICc ="~.(paste(round(M3.tab2$Delta_AICc[M3.tab2$Modnames=="ndvi"]-M3.tab2$Delta_AICc[M3.tab2$Modnames=="null"],2),sep="")))),adj=0,col="red", cex=0.9)
mtext(text="(b)", side = 3, line = 0.5, adj = 0, cex = 1)



#maps = plot of mean ndvi values over time, second plot of variation in ndvi values
summary(platystat$ndvi,platystat$year)

head(platystat);dim(platystat)
plot(platystat$year,platystat$ndvi)
?par

#For appendix
dev.new(width=8, height=4, dpi=80, pointsize=14, noRStudioGD = T)
par(mfrow=c(1,2), mar=c(4.5,4,1,1), mgp=c(2.8,0.8,0), oma=c(0,0,1,0))

plot(platystat$year,platystat$ndbi, pch=20, ylab="Mean ndbi",xlab="Year", las = 1,cex.axis=0.75,cex.lab=0.75,col=factor(platystat$catchment))
colors <- c("purple2",
            "deepskyblue2",
            "gold2",
            "deeppink1",
            "green2")
legend("topleft",
       legend = levels(factor(platystat$catchment)),
       pch = 20, bg="white",
       col = factor(levels(factor(platystat$catchment))))
mtext(text="(a)", side = 3, line = 0.5, adj = 0, cex = 1)


plot(platystat$year,platystat$stdev, pch=20, ylab="ndvi stdev",xlab="Year", las = 1,cex.axis=0.75,cex.lab=0.75,col=factor(platystat$catchment))
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
T1<-table(platystat$year,platystat$occurrence)
T2<-T1[,2]/(T1[,1]+T1[,2])
T3<-data.frame(year=names(T2),platy=T2)
plot(T3$year,T3$platy)

