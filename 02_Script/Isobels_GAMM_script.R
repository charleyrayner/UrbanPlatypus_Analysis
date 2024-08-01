
library(mgcv)

# Define the k term in the gamm model?
gammmod1 <- gamm(all~s(Yr, k=5), random=list(Reserve=~1), data=fire_abun5, family=poisson)
summary(gammmod1$gam)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(gammmod1$gam,pages=1)

# How do we do model selection using gamms (e.g. AICc). To do model selection you can use a) p values, b) Likelihood ratio test (anova in r), c) AIC. E.g Compare the smooth gam to the non-smooth gam.

gammmod1.a <- gamm(all~Yr, random=list(Reserve=~1), data=fire_abun5, family=poisson)
summary(gammmod1.a$gam)

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

newdf <- data.frame(Yr=seq(0,5,length.out=100))
mod1_pr<-predict(gammmod1$gam,newdata=newdf,se.fit=T,type='response')
newdf$fit<-mod1_pr$fit
newdf$se.fit<-mod1_pr$se.fit
newdf$lci <- newdf$fit-(newdf$se.fit*1.96)
newdf$uci <- newdf$fit+(newdf$se.fit*1.96)
str(mod1_pr)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$Yr,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),las=1,cex=1.5,xaxt='n',ylab='Species richness',xlab='Year')
arrows(newdf$Yr,newdf$lci,newdf$Yr,newdf$uci,length=0.1,angle=90,code=3)

# Isobel's line plot:
dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$Yr,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),xlim=c(0,5),las=1,cex=1.5,xaxt='n',ylab='Species richness',xlab='Year',main='All species',type='l')
axis(side=1,at=c(0,1,2,3,4,5),labels=c('2015','2016','2017','2018','2019','2020'))
lines(newdf$Yr,newdf$uci,lty=2)
lines(newdf$Yr,newdf$lci,lty=2)

# Plot with polygon CI:

dev.new(height=5,width=10,dpi=80,pointsize=26,noRStudioGD = T)
par(mar=c(2.5,5,0.5,0.5), mgp=c(2.5,1,0))
plot(newdf$Yr,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),xlim=c(0,5),las=1,cex=1.5,xaxt='n',ylab='Plant richness',xlab='',main='',type='l',lwd=2,cex.axis=1.25, cex.lab=1.2)
axis(side=1,at=c(0,1,2,3,4,5),labels=c('2015','2016','2017','2018','2019','2020'), cex.axis=1.25)
pg.ci(x="Yr","newdf",colour=rgb(0,0,0,0.2), lower="lci",upper="uci")

# save.image("04_Workspaces/fire_analysis.RData")
