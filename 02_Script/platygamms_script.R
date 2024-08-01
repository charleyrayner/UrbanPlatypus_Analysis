install.packages("lme4")
library(lme4)
library(AICcmodavg)
#download package mgcv (generalised addditive mixed model GAMMs)
install.packages("mgcv")
library(mgcv)
library(AICcmodavg)

# Define the k term in the gamm model?
#model using gamm with year as a linear term
gammmod1 <- gamm(occurrence~s(year, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)
# How do we do model selection using gamms (e.g. AICc). To do model selection you can use a) p values, b) Likelihood ratio test (anova in r), c) AIC. E.g Compare the smooth gam to the non-smooth gam.
gammmod1.a <- gamm(occurrence~year, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(gammmod4$gam,pages=1)

#model using gamm with year and ndvi as linear terms
gammmod2 <- gamm(occurrence~s(year*ndvi, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod2.a <- gamm(occurrence~year*ndvi, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#model using gamm with year as a linear term and ndvi as an additive effect
gammmod3 <- gamm(occurrence~s(year+ndvi, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod3.a <- gamm(occurrence~year+ndvi, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#model using glmer with year and ndbi as linear terms
gammmod4 <- gamm(occurrence~s(year*ndbi, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod4.a <- gamm(occurrence~year*ndbi, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#model using glmer with year as a linear term and ndbi as an additive effect
gammmod5 <- gamm(occurrence~s(year+ndbi, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod5.a <- gamm(occurrence~year+ndbi, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#model using glmer with year and LC as linear terms
gammmod6 <- gamm(occurrence~s(year*LC, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod6.a <- gamm(occurrence~year*LC, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#model using glmer with year as a linear term and LC as an additive effect
gammmod7 <- gamm(occurrence~s(year+LC, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammmod7.a <- gamm(occurrence~year+LC, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#null model (doesn't work)
gammNull <- gamm(occurrence~s(1, k=5), random=list(catchment=~1,siteID=~1), data=platystatCatchment, family=binomial)

gammNull.a <- gamm(occurrence~1, random=list(catchment=~1, siteID=~1), data=platystatCatchment, family=binomial)


#summaries
summary(gammmod1$gam)
summary(gammmod1.a$gam)
summary(gammmod2$gam)
summary(gammmod2.a$gam)
summary(gammmod3$gam)
summary(gammmod3.a$gam)
summary(gammmod4$gam)
summary(gammmod4.a$gam)
summary(gammmod5$gam)
summary(gammmod5.a$gam)
summary(gammmod6$gam)
summary(gammmod6.a$gam)
summary(gammmod7$gam)
summary(gammmod7.a$gam)

#AICc comparison doesn't work
AICc(gammmod1$gam)


#Graph model

newdf <- data.frame(year=seq(0,9,length.out=100))
mod1_pr<-predict(gammmod1$gam,newdata=newdf,se.fit=T,type='response')
newdf$fit<-mod1_pr$fit
newdf$se.fit<-mod1_pr$se.fit
newdf$lci <- newdf$fit-(newdf$se.fit*1.96)
newdf$uci <- newdf$fit+(newdf$se.fit*1.96)
str(mod1_pr)

# not sure why this is so linear? 
dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$year,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),las=1,cex=1.5,xaxt='n',ylab='occurrence',xlab='Year')
arrows(newdf$year,newdf$lci,newdf$year,newdf$uci,length=0.1,angle=90,code=3)

dev.new(height=6,width=12,dpi=80,pointsize=12,noRStudioGD = T)
plot(newdf$year,newdf$fit,ylim=c(min(newdf$lci), max(newdf$uci)),xlim=c(0,10),las=1,cex=1.5,xaxt='n',ylab='Occurrence prob',xlab='Year',main='Platypus',type='l')
axis(side=1,at=c(0,1,2,3,4,5,6,7,8,9,10),labels=c('2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023'))
lines(newdf$year,newdf$uci,lty=2)
lines(newdf$year,newdf$lci,lty=2)


