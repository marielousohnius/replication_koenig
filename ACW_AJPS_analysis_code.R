####
# This code performs all the analysis
# appearing in the manuscript and supplemental materials
# for "The Political Consequences of External Economic Shocks"
# Ahlquist, Copelovitch, & Walter. American Journal of Political Science
# Install the following files in your working directory:
# PLN_CHF_EUR_FXdata.csv [Data on exchange rates]
# AJPSimputations.RData [Dataset containing the complete set of imputed survey data]
# last updated: 20 October 2019
# R version 3.3.1
# Other attached packages:
# vioplot_0.2, sm_2.2-5.4, mlogit_0.4-1, lmtest_0.9-35, readstata13_0.9.0, 
# Amelia_1.7.4, Rcpp_1.0.1, zoo_1.8-1, lubridate_1.7.1, xtable_1.8-2, texreg_1.36.23,    
# stargazer_5.2.1, RColorBrewer_1.1-2, nnet_7.3-12, cem_1.1.19, weights_0.85, mice_2.46.0, gdata_2.18.0, 
# Hmisc_4.1-1, ggplot2_3.1.0, Formula_1.2-2, survival_2.41-3,   
# lattice_0.20-35, MultinomialCI_1.0, foreign_0.8-69, MASS_7.3-51.4
#####

rm(list=ls())
gc()
library(MASS)
library(foreign)
library(MultinomialCI)
library(weights)
library(cem)
library(nnet)
library(RColorBrewer)
library(stargazer)
library(texreg)
library(xtable)
library(lubridate)
library(zoo)
library(Amelia)
library(readstata13)
library(lattice)
library(mlogit)
library(Hmisc)
library(vioplot)

#setwd()  #set to your wd
dir.create("./tables_figs") #subdirectory for main text output
dir.create("./tables_figs/SM") #subdirectory for supplemental materials output

rng<-5879521
set.seed(rng)
inv.logit<-function(x){exp(x)/(exp(x)+1)}

#Figure 1
fxdat<-read.csv("PLN_CHF_EUR_FXdata.csv")
fxdat$YYYY.MM.DD<-as.Date(fxdat$YYYY.MM.DD)
pdf("./tables_figs/Fig1.pdf",width=11, height=6) #Figure 1
par(mfrow=c(1,2))
plot(fxdat$YYYY.MM.DD, fxdat$CHF.EUR, type="l", ylim=c(.95,1.25),
	las=1, lwd=2, col=grey(0.5), main="Euro/Swiss franc exchange rate", 
	ylab="EUR/CHF", xlab="2014-2015", bty="n", xaxt="n")
axis(1, at = as.Date(c("2014-11-01","2015-01-01", "2015-03-01", "2015-05-01", 
	"2015-07-01", "2015-09-01", "2015-11-01")), 
	labels=c("Nov", "Jan", "Mar", "May", "Jul", "Sep", "Nov"))
plot(fxdat$YYYY.MM.DD, fxdat$CHF.PLN, type="l", ylim=c(.20,.30),
	las=1, lwd=2, main="Polish zloty/Swiss franc exchange rate", 
	ylab="PLN/CHF", xlab="2014-2015", bty="n", xaxt="n")
axis(1, at = as.Date(c("2014-11-01","2015-01-01", "2015-03-01", "2015-05-01", 
	"2015-07-01", "2015-09-01", "2015-11-01")), 
	labels=c("Nov", "Jan", "Mar", "May", "Jul", "Sep", "Nov"))
dev.off()
rm(fxdat)

load("AJPSimputations.RData")
# creating variables for analysis
## simplified vote choice variable that lumps those unsure about voting with abstainers
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$vote.intent.rev2<-imp.out$imputations[[i]]$vote.intent.rev
	imp.out$imputations[[i]]$vote.intent.rev2[imp.out$imputations[[i]]$turnout.intent.ord<2]<-"NoVote"
	imp.out$imputations[[i]]$vote.intent.rev2<-relevel(imp.out$imputations[[i]]$vote.intent.rev2, ref="PO/PSL")
	
}

## simplified vote choice variable that lumps those unsure about voting or unsure about party with abstainers
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$vote.intent.rev3<-"NoVote"
	imp.out$imputations[[i]]$vote.intent.rev3[imp.out$imputations[[i]]$vote.intent.rev2=="PO/PSL"]<-"PO/PSL"
	imp.out$imputations[[i]]$vote.intent.rev3[imp.out$imputations[[i]]$vote.intent.rev2=="other"]<-"other"
	imp.out$imputations[[i]]$vote.intent.rev3[imp.out$imputations[[i]]$vote.intent.rev2=="PiS"]<-"PiS"
	imp.out$imputations[[i]]$vote.intent.rev3<-relevel(as.factor(imp.out$imputations[[i]]$vote.intent.rev3), ref="PO/PSL")
}


##single FX variable
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$FXstatus<-"none"
	imp.out$imputations[[i]]$FXstatus[imp.out$imputations[[i]]$currentFXloan==1]<-"exposed"
	imp.out$imputations[[i]]$FXstatus[imp.out$imputations[[i]]$pastFXloan==1]<-"past"
	imp.out$imputations[[i]]$FXstatus<-factor(imp.out$imputations[[i]]$FXstatus,
	 levels=c("none","exposed", "past"))
	imp.out$imputations[[i]]$FXstatus<-relevel(imp.out$imputations[[i]]$FXstatus, ref = "none")
}


## current borrowers are exposed 
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$exposed<-as.numeric(imp.out$imputations[[i]]$FXstatus=="exposed")
}

## reordering treatment levels & PS 
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$treat<-factor(imp.out$imputations[[i]]$treat, 
		levels=c("cntrl", "info", "history", "Hungary"))
	imp.out$imputations[[i]]$ps<-factor(imp.out$imputations[[i]]$ps, 
		levels=c("DK", "none", "some", "50/50", "90/10"))
	imp.out$imputations[[i]]$ps<-relevel(imp.out$imputations[[i]]$ps, 
		ref="none")
	imp.out$imputations[[i]]$gi<-relevel(imp.out$imputations[[i]]$gi, ref="DK")
}

## reversing religiousity scale; bigger is more religious 
for(i in 1:imp.out$m){
	imp.out$imputations[[i]]$religion<- 6 -1*imp.out$imputations[[i]]$religious_participation
}


##observed data, no imputations
obs.dat<-imp.out$imputations[[1]][,1:ncol(imp.out$missMatrix)]
obs.dat[imp.out$missMatrix==1]<-NA
obs.dat$FXstatus<-"none"
obs.dat$FXstatus[obs.dat$currentFXloan==1]<-"exposed"
obs.dat$FXstatus[obs.dat$pastFXloan==1]<-"past"
obs.dat$FXstatus<-factor(obs.dat$FXstatus, levels=c("none","exposed","past"))
obs.dat$exposed<-obs.dat$FXstatus=="exposed"
obs.dat$gi<-factor(obs.dat$gi, levels=c("DK","none","some","big"))
obs.dat$gi<-relevel(obs.dat$gi, ref="DK")
obs.dat$ps<-factor(obs.dat$ps, levels=c("DK","none","some","50/50","90/10"))
obs.dat$ps<-relevel(obs.dat$ps, ref="none")
obs.dat$vote.intent.rev2<-obs.dat$vote.intent.rev
obs.dat$vote.intent.rev2[obs.dat$turnout.intent.ord<2]<-"NoVote"
obs.dat$vote.intent.rev3<-obs.dat$vote.intent.rev2
obs.dat$vote.intent.rev3[obs.dat$vote.intent.rev2=="DK"]<-"NoVote"
obs.dat$vote.intent.rev3<-droplevels(obs.dat$vote.intent.rev3)
obs.dat$gi.bin<-as.numeric(obs.dat$gi %in% c("some", "big"))
obs.dat$age.expand <- as.factor(cut2(obs.dat$age, g=5))
obs.dat$age.expand<-relevel(obs.dat$age.expand, ref="[32,44)")
obs.dat$LRp1<-1+obs.dat$LeftRight #shifting the scale up 1 for plotting later
obs.dat$LeftRightCat<-"center"
obs.dat$LeftRightCat[obs.dat$LeftRight<0]<-"left"
obs.dat$LeftRightCat[obs.dat$LeftRight>0]<-"right"
obs.dat$LeftRightCat<-as.factor(obs.dat$LeftRightCat)
obs.dat$religion<- 6-1*obs.dat$religious_participation
obs.dat$treat<-factor(obs.dat$treat, 
		levels=c("cntrl", "info", "history", "Hungary"))	

### merging immigration questions; not part of imputation or analysis
load("AJPScleanedData.Rdata")
obs.dat$immigration.summary<- octdat$immigration.summary
obs.dat$antimigrant.summary<- octdat$antimigrant.summary
rm(octdat)

##PiS survey vote choice est.
PiS.vs.surv<-sum(obs.dat$weight[obs.dat$vote.intent.rev3=="PiS"], na.rm=T)/sum(obs.dat$weight[obs.dat$vote.intent.rev3!="NoVote"], na.rm=T)
PiS.vs.surv

# plots of response distribution proportions
## GI x FX: Figure A.1  
sumtab.gi<-table(obs.dat$FXstatus,obs.dat$gi)
sumtab.gi<-sumtab.gi[c(2,3,1),]
rowprop.gi<-sweep(sumtab.gi, 1, margin.table(sumtab.gi, 1), "/")
ci.gi<-NULL
for(i in 1:nrow(rowprop.gi)){ci.gi[[i]]<-multinomialCI(sumtab.gi[i,], alpha=0.05)}
pdf("./tables_figs/SM/FigA1.pdf", width=11, height = 6)
#pdf("./tables_figs/GIxExposedPropAJPS.pdf", width = 8, height=6.3)
	xpts<-c(0,1,2,3)
barCenters<-rbind(xpts-.12, xpts, xpts+.12)
par(mfrow=c(1,2))
plot(barCenters[1,],rowprop.gi[1,], 
	ylim=c(0,0.8), bty="n", xaxt = "n", cex=1.25, pch=16, 
	xlim=c(-.5,3.5), ylab="proportion of borrower group", xlab="", las=1,
	main= "Support for government invervention")
axis(1,at=xpts, labels = c("DK", "none", "some", "big"))
points(barCenters[2,],rowprop.gi[2,], cex=1.25, pch=17, col=grey(0.7))
points(barCenters[3,],rowprop.gi[3,], cex=1.25, pch=18, col=grey(0.7))
arrows(barCenters[1,],ci.gi[[1]][,1], barCenters[1,], ci.gi[[1]][,2],
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
arrows(barCenters[2,],ci.gi[[2]][,1], barCenters[2,], ci.gi[[2]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
arrows(barCenters[3,],ci.gi[[3]][,1], barCenters[3,], ci.gi[[3]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
legend(x="top", legend=c("exposed", "past borrower", "never borrowed"),
	pch=c(16,17,18), col=c("black",grey(0.7),grey(0.7)), lty=1, bty="n",
	cex=.7, horiz=TRUE)
dev.off()

## PS x FX: Figure 2
sumtab.ps<-table(obs.dat$FXstatus,obs.dat$ps)
sumtab.ps<-sumtab.ps[c(2,3,1),c(2,1,3,4,5)]
rowprop.ps<-sweep(sumtab.ps, 1, margin.table(sumtab.ps, 1), "/")
ci.ps<-NULL
for(i in 1:nrow(sumtab.ps)){ci.ps[[i]]<-multinomialCI(sumtab.ps[i,], alpha=0.05)}

pdf("./tables_figs/Fig2.pdf", width = 8, height=6.3)
xpts<-0:4
barCenters<-rbind(xpts-.12, xpts, xpts+.12)
plot(barCenters[1,],rowprop.ps[1,], 
	ylim=c(0,0.8), bty="n", xaxt = "n", cex=1.25, pch=16, 
	xlim=c(-.5,4.5), ylab="proportion of borrower group", xlab="", las=1)
axis(1,at=xpts, labels = c("DK", "none", "some", "50/50", "90/10"))
points(barCenters[2,],rowprop.ps[2,], cex=1.25, pch=17, col=grey(0.7))
points(barCenters[3,],rowprop.ps[3,], cex=1.25, pch=18, col=grey(0.7))
arrows(barCenters[1,],ci.ps[[1]][,1], barCenters[1,], ci.ps[[1]][,2],
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
arrows(barCenters[2,],ci.ps[[2]][,1], barCenters[2,], ci.ps[[2]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
arrows(barCenters[3,],ci.ps[[3]][,1], barCenters[3,], ci.ps[[3]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
legend(x="top", legend=c("exposed", "past borrower", "never borrowed"),
	pch=c(16,17,18), col=c("black",grey(0.7),grey(0.7)), lty=1, bty="n",
	cex=.8, horiz=TRUE)
dev.off()


# Figure 4: comparing FX borrowers with core incumbent & PO voters
tab.comp<-data.frame(matrix(0, nrow=6, ncol=4))
colnames(tab.comp)<-c("2011 PiS voters", "2011 PO/PSL voters", "FX-exposed", "Past borrower")
rownames(tab.comp)<-c("income", "urban", "education", "Left-Right", "religious", "anti-immigrant")
tab.comp[,1]<-apply(obs.dat[(obs.dat$PiS2011==1 & obs.dat$FXstatus=="none"), #PiS voters
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
tab.comp[,2]<-apply(obs.dat[(obs.dat$POPSL2011==1 & obs.dat$FXstatus=="none"), #Gov't voters
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
tab.comp[,3]<-apply(obs.dat[obs.dat$FXstatus=="exposed", 
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
tab.comp[,4]<-apply(obs.dat[obs.dat$FXstatus=="past", 
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )

##bootstrap CIs
B<-10000
n<-nrow(obs.dat)
bs.out<-array(numeric(),c(6,4,B))
dimnames(bs.out)[c(1,2)]<-list(rownames(tab.comp),colnames(tab.comp))
for(i in 1:B){
	temp.dat<-obs.dat[sample(1:n,n,replace=T),]
	temp.tab<-matrix(0, ncol=4, nrow=6)
	temp.tab[,1]<-apply(temp.dat[(temp.dat$PiS2011==1 & temp.dat$FXstatus=="none"), #PiS voters
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
	temp.tab[,2]<-apply(temp.dat[(temp.dat$POPSL2011==1 & temp.dat$FXstatus=="none"), #Gov't voters
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
	temp.tab[,3]<-apply(temp.dat[temp.dat$FXstatus=="exposed", 
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
	temp.tab[,4]<-apply(temp.dat[temp.dat$FXstatus=="past", 
	c("income.quint", "urban_rural", "ed_level", "LRp1",
		"religion", "antimigrant.summary")],2,median, na.rm=T )
	bs.out[,,i]<-temp.tab
}
cis<-apply(bs.out, c(1,2), quantile, c(.025, .975), na.rm=T)

xpts<-c(0,3,6,9,12,15)
barCenters<-matrix(sort(c(xpts-.75, xpts-.25, xpts+.25, xpts+.75)), ncol=4, byrow=T)
pdf("./tables_figs/Fig4.pdf", width=8, height = 6.3)
plot(barCenters[,1],tab.comp[,1], 
	ylim=c(.9,5.5), bty="n", xaxt = "n", cex=1.3, pch=15, col="#000080", 
	xlim=c(-.5,15.5), ylab="median score", xlab="", las=1)
axis(1,at=xpts, labels = rownames(tab.comp))
abline(v=xpts+1.5, col=grey(0.85), lty=3)
points(barCenters[,2],tab.comp[,2], cex=1.3, pch=17, col="orange")
points(barCenters[,3],tab.comp[,3], cex=1.3, pch=16)
points(barCenters[,4],tab.comp[,4], cex=1.3, pch=23, col=grey(0.7), bg=grey(0.7))

arrows(barCenters[,1],cis[,,1][1,], barCenters[,1], cis[,,1][2,],
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05, col="#000080")
arrows(barCenters[,2],cis[,,2][1,], barCenters[,2], cis[,,2][2,],
       lwd = 1.5, angle = 90, col="orange",
       code = 3, length = 0.05)
arrows(barCenters[,3],cis[,,3][1,], barCenters[,3], cis[,,3][2,],
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
arrows(barCenters[,4],cis[,,4][1,], barCenters[,4], cis[,,4][2,],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)

legend(x="top", legend=c("PiS 2011", "PO/PSL 2011", "exposed", "past borrower"),
	pch=c(15,17,16,23), col=c("#000080", "orange","black",grey(0.7)), 
	pt.bg=c(NA, NA,NA, grey(0.7)), bty="n", cex=.9, horiz=TRUE)
dev.off()


# Models
## Selection into FX mortgage, for SM
aggregate(age~FXstatus, data=obs.dat, FUN=median, na.rm=T) #median age for borrowers

b.cfx<-se.cfx<-cfx.out<-b.pfx<-se.pfx<-pfx.out<-NULL
for(i in 1:imp.out$m) {
	cfx.out[[i]] <- glm(currentFXloan==1~age.expand + female + 
		married + income.quint + ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		family=binomial,
		weight = weight, 
		data = imp.out$imputations[[i]])	
	b.cfx <- rbind(b.cfx, cfx.out[[i]]$coef)
	se.cfx <- rbind(se.cfx, coef(summary(cfx.out[[i]]))[,2])
	pfx.out[[i]] <- glm(pastFXloan==1~age.expand + female + 
		married + income.quint + ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		family=binomial,
		weight = weight, 
		data = imp.out$imputations[[i]])	
	b.pfx <- rbind(b.pfx, pfx.out[[i]]$coef)
	se.pfx <- rbind(se.pfx, coef(summary(pfx.out[[i]]))[,2])
}
cfx.comb <- mi.meld(q = b.cfx, se = se.cfx)
pfx.comb <- mi.meld(q = b.pfx, se = se.pfx)

fx.coef<- list(as.vector(cfx.comb$q.mi), as.vector(pfx.comb$q.mi))
fx.se<- list(as.vector(cfx.comb$se.mi), as.vector(pfx.comb$se.mi))


cov.names<- c("(Intercept)",
	"18-31", "44-56", "57-66", "66+", "female", "married", "income", "education",
	"urban", "employed", "religiosity", "Household size", 
	"Left", "Right"
	#rep("province",15)
	)

screenreg(l=list(cfx.out[[1]], pfx.out[[1]]),
	omit.coef="province",
	override.coef = fx.coef,
	override.se = fx.se,
	stars = numeric(0),
	include.aic = FALSE, 
    include.bic = FALSE, include.loglik = FALSE
	)

htmlreg(l=list(cfx.out[[1]], pfx.out[[1]]),
	file = "./tables_figs/SM/TabA1.html",
	override.coef = fx.coef,
	override.se = fx.se,
	omit.coef="province",
	custom.coef.names = cov.names,
	digits = 2,
	stars=numeric(0),
	custom.note = "All models include province dummies (omitted from table).", 
	caption.above = TRUE,
	caption = "Selection into FX loans, current and past.  Weighted logistic regression over 20 imputed datasets.",
	html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
	include.aic = FALSE, 
    include.bic = FALSE, include.loglik = FALSE)

## Government intervention models, Table A-3 & A-4
b.gi3.out<-se.gi3.out<-gi3.out<-NULL #model 1 
for(i in 1:imp.out$m) {
	gi3.out[[i]] <- glm(gi.bin ~ treat,
		family=binomial,
		weight = weight, 
		data = imp.out$imputations[[i]])
	b.gi3.out <- rbind(b.gi3.out, gi3.out[[i]]$coef)
	se.gi3.out <- rbind(se.gi3.out, coef(summary(gi3.out[[i]]))[,2])
}
gi3.comb <- mi.meld(q = b.gi3.out, se = se.gi3.out)


b.gi4.out<-se.gi4.out<-gi4.out<-NULL #model 2 
for(i in 1:imp.out$m) {
	gi4.out[[i]] <- glm(gi.bin ~ treat + FXstatus,
		family=binomial,
		weight = weight, 
		data = imp.out$imputations[[i]])
	b.gi4.out <- rbind(b.gi4.out, gi4.out[[i]]$coef)
	se.gi4.out <- rbind(se.gi4.out, coef(summary(gi4.out[[i]]))[,2])
}
gi4.comb <- mi.meld(q = b.gi4.out, se = se.gi4.out)


b.gi5.out<-se.gi5.out<-gi5.out<-NULL #model 3 
for(i in 1:imp.out$m) {
	gi5.out[[i]] <- glm(gi.bin ~ treat +  FXstatus + age.expand +  female + 
		married + income.quint + ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		family=quasibinomial,
		weight = weight, 
		data = imp.out$imputations[[i]])
	b.gi5.out <- rbind(b.gi5.out, gi5.out[[i]]$coef)
	se.gi5.out <- rbind(se.gi5.out, coef(summary(gi5.out[[i]]))[,2])
}
gi5.comb <- mi.meld(q = b.gi5.out, se = se.gi5.out)

b.gi6.out<-se.gi6.out<-gi6.out<-NULL #model 3i 
for(i in 1:imp.out$m) {
	gi6.out[[i]] <- glm(gi.bin ~ treat*exposed + age.expand +  female + 
		married + income.quint + ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		family=quasibinomial,
		weight = weight, 
		data = imp.out$imputations[[i]])
	b.gi6.out <- rbind(b.gi6.out, gi6.out[[i]]$coef)
	se.gi6.out <- rbind(se.gi6.out, coef(summary(gi6.out[[i]]))[,2])
}
gi6.comb <- mi.meld(q = b.gi6.out, se = se.gi6.out)

gi.coef<-list( 
	as.vector(gi3.comb$q.mi), as.vector(gi4.comb$q.mi), 
	as.vector(gi5.comb$q.mi), as.vector(gi6.comb$q.mi))

gi.se<-list(
	as.vector(gi3.comb$se.mi), as.vector(gi4.comb$se.mi), 
	as.vector(gi5.comb$se.mi), as.vector(gi6.comb$se.mi))
gi.p<-list()
for(i in 1:length(gi.se)){
	gi.p[[i]]<- 2*(1-pnorm(abs(gi.coef[[i]]/gi.se[[i]])))
}

cov.names<- c("(Intercept)", "information", "history", "Hungary",
	"FX-exposed", "past borrower", "18-31", "44-56", "57-65", "66+", 
	"female", "married", "income", "education",
	"urban", "employed", "religiosity", "household size", 
	"Left", "Right", "FX-exposed", "exposed x info", "exposed x history", "exposed x Hungary")

screenreg(l=list(gi3.out[[1]], gi4.out[[1]], gi5.out[[1]], gi6.out[[1]]),
	override.coef = gi.coef,
	override.se = gi.se,
	override.pvalues = gi.p,
	omit.coef="province",
	custom.coef.names = cov.names,
	digits = 2,
	stars= c(0.1, 0.05),
	include.aic = F, 
    include.bic = F, include.loglik = F, include.deviance = F)

htmlreg(l=list(gi3.out[[1]], gi4.out[[1]], gi5.out[[1]], gi6.out[[1]]),  #Table A-3
	file = "./tables_figs/SM/TabA3.html",
	override.coef = gi.coef,
	override.se = gi.se,
	override.pvalues = gi.p,
	omit.coef="province",
	custom.coef.names = cov.names,
	digits = 2,
	stars=0,
	caption = "",
	html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
	include.aic = FALSE, include.deviance = FALSE,
    include.bic = FALSE, include.loglik = FALSE)

### Interpretive quantities described older versions
inv.logit(gi.coef[[1]]%*%c(1,0,0,0))
inv.logit(gi.coef[[1]]%*%c(1,1,0,0))
inv.logit(gi.coef[[2]]%*%c(1,0,0,0,1,0))

### multinomial models for the Appendix.
gi.basic.mult<-multinom(gi ~ treat, Hess = TRUE, weights = weight, data = obs.dat)
gi.mult<-multinom(gi ~ treat + FXstatus, Hess = TRUE, weights = weight, data = obs.dat)

cov.names<- c("(Intercept)", "information", "history", "Hungary",
	"FX-exposed", "past borrower")

htmlreg(l=list(gi.basic.mult, gi.mult),  #Table A-4
	file = "./tables_figs/SM/TabA4.html",
	custom.coef.names = cov.names,
	digits = 2,
	stars= 0,
	#caption.above = TRUE,
	#caption = "Preference for government intervention.  Weighted logistic regression over 20 imputed datasets.",
	caption = "",
	html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
	include.aic = FALSE, include.deviance = FALSE,
    include.bic = FALSE, include.loglik = FALSE)


## proposal support models
b.ps1.out<-se.ps1.out<-ps1.out<-NULL #model 1  for table 1
for(i in 1:imp.out$m) {
	ps1.out[[i]] <- multinom(ps ~ treat + 
		age.expand + female + married + income.quint + 
		ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		weight = weight, Hess=TRUE, maxit = 200,
		data = imp.out$imputations[[i]], model=T)
	b.ps1.out <- rbind(b.ps1.out, as.vector(coef(ps1.out[[i]])))
	se.ps1.out <-rbind(se.ps1.out, as.vector(summary(ps1.out[[i]])$standard.errors))
}
ps1.comb <- mi.meld(q = b.ps1.out, se = se.ps1.out)

b.ps2.out<-se.ps2.out<-ps2.out<-NULL #model 2 for table 1
for(i in 1:imp.out$m) {
	ps2.out[[i]] <- multinom(ps ~ treat + FXstatus +
		age.expand + female + married + income.quint + 
		ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province,
		weight = weight, Hess=TRUE, maxit = 200,
		data = imp.out$imputations[[i]], model=T)
	b.ps2.out <- rbind(b.ps2.out, as.vector(coef(ps2.out[[i]])))
	se.ps2.out <-rbind(se.ps2.out, as.vector(summary(ps2.out[[i]])$standard.errors))
}
ps2.comb <- mi.meld(q = b.ps2.out, se = se.ps2.out)


cov.names<- c("information", "history", "Hungary",
	"FX-exposed", "past borrower",
	"18-31", "44-56", "57-66", "66+", "female", "married", "income", "education",
	"urban", "employed", "religiosity", "Household size", 
	"Left", "Right", "(Intercept)"    
	)


stargazer(ps1.out[[1]],ps2.out[[1]],  #results for Table 1 & A-5
	type="html",
	coef= list(matrix(ps1.comb$q.mi, nrow=4), matrix(ps2.comb$q.mi, nrow=4)),
	se = list(matrix(ps1.comb$se.mi, nrow=4),matrix(ps2.comb$se.mi, nrow=4)),
	#p = list(matrix(pnorm(abs(ps1.comb$q.mi/ps1.comb$se.mi), lower=F), nrow=4),
	#	matrix(pnorm(abs(ps2.comb$q.mi/ps2.comb$se.mi), lower=F), ncol=4)),
	omit = "province",
	column.labels = c("DK", "some", "50/50", "90/10", "DK", "some", "50/50", "90/10"),
	covariate.labels = cov.names,
	digits = 2,
	star.cutoffs = 0,
	style="ajps",
	nobs=TRUE,
	out = "./tables_figs/Tab1.html",
	table.layout = "#tncm-"
	)
#p.values for table 1/A-5 model 1
p.vals.t1.m1<-t(matrix(round(2*pnorm(abs(ps1.comb$q.mi/ps1.comb$se.mi), lower=F),2), nrow=4))
colnames(p.vals.t1.m1)<-c("DK", "some", "50/50", "90/10")
rownames(p.vals.t1.m1)<-ps1.out[[1]]$coefnames
#p.values for table 1/A-5 model 2
p.vals.t1.m2<-t(matrix(round(2*pnorm(abs(ps2.comb$q.mi/ps2.comb$se.mi), lower=F),2), nrow=4))
colnames(p.vals.t1.m2)<-c("DK", "some", "50/50", "90/10")
rownames(p.vals.t1.m2)<-ps2.out[[1]]$coefnames

###first diff/interpretation quantities
X.ne.cntrl<-X.ne.info<-X.ne.hun<-X.ne.hist<-X.e.cntrl<-ps2.out[[i]]$model[1,]
X.ne.cntrl[1,]<-X.ne.info[1,]<-X.ne.hun[1,]<-X.ne.hist[1,]<-X.e.cntrl[1,]<-data.frame(
	"DK","cntrl", #cntrl group respondent
	"none", #no FX borrowing
	"[32,44)", #reference age group (near-modal)
	1, # female (median respondent)
	1, #married (median respondent)
	3, #median income quintile
	2, #median education level
	2, #median settlement size
	2, #median employment value
	4, #median religiosity
	3, #median household size
	"right", #modal political persuasion
	"Mazowieckie", #modal province
	1) #weight)
X.ne.info[2]<-"info"
X.ne.hun[2]<-"Hungary"
X.ne.hist[2]<-"history"
X.e.cntrl[3]<-"exposed"

pp.ne.cntrl<-pp.ne.info<-pp.ne.hun<-pp.ne.hist<-pp.e.cntrl<-NULL

for(i in 1:imp.out$m){
	pp.ne.cntrl<-rbind(pp.ne.cntrl, predict(ps2.out[[i]], newdata=X.ne.cntrl,type="probs"))
	pp.ne.info<-rbind(pp.ne.info, predict(ps2.out[[i]], newdata=X.ne.info,type="probs"))
	pp.ne.hun<-rbind(pp.ne.hun, predict(ps2.out[[i]], newdata=X.ne.hun,type="probs"))
	pp.ne.hist<-rbind(pp.ne.hist, predict(ps2.out[[i]], newdata=X.ne.hist,type="probs"))
	pp.e.cntrl<-rbind(pp.e.cntrl, predict(ps2.out[[i]], newdata=X.e.cntrl,type="probs"))
}

apply(pp.ne.cntrl,2,mean)
apply(pp.ne.info,2,mean)
apply(pp.ne.hun,2,mean)
apply(pp.ne.hist,2,mean)
apply(pp.e.cntrl,2,mean)

#### pred prob plot; Fig 3
X.pred<-model.matrix(ps ~ treat + FXstatus +
		age.expand + female + married + income.quint + 
		ed_level + urban_rural + employed + 
		religion + hh_size + LeftRightCat + province, 
	data=rbind(X.ne.cntrl, X.ne.info, X.ne.hist, X.ne.hun,X.e.cntrl))
probs.9010<-NULL
probs.none<-NULL
l<-nrow(coef(ps2.out[[1]]))
for(i in 1:imp.out$m){
	b<- as.vector(t(coef(ps2.out[[i]]))) #vectorizing coef matrix to match vcov
	B.ps<-mvrnorm(500, b, vcov(ps2.out[[i]])) #drawing 500 coef. vectors for each imputation
	for(j in 1:nrow(B.ps)){ #getting predicted 90/10 support probability 
		B<-matrix(B.ps[j,], nrow = l, byrow=T)
		lp.cf<-B%*%t(X.pred)
		denom.cf<-1+colSums(exp(lp.cf))
		pp.mat.cf<-sweep(exp(lp.cf), 2, denom.cf, "/") #predicted probs
		pp.mat.cf<-rbind(pp.mat.cf, 1-colSums(pp.mat.cf))
		rownames(pp.mat.cf)<-c("DK", "some", "50/50", "90/10", "none")
		probs.9010<-rbind(probs.9010,pp.mat.cf[4,])
		probs.none<-rbind(probs.none,pp.mat.cf[5,])
	}
}
colnames(probs.9010)<-colnames(probs.none)<-c("cntrl", "info", "history", "Hungary", "exposed")

pdf("./tables_figs/Fig3.pdf", width = 11, height=6)
par(mfrow = c(1,2))
plot(density(probs.none[,2]-probs.none[,1], adjust=3), col=grey(0.6), xlim=c(-.4,.1),
	xlab="change in predicted Pr('none') relative to unexposed control", ylab = "density",
	bty="n", las=1, lwd=2, ylim=c(0,12), main = "Least generous policy (none)")
lines(density(probs.none[,3]-probs.none[,1], adjust=3), lwd=2, lty=2, col=grey(0.6))
lines(density(probs.none[,4]-probs.none[,1], adjust=3), lty=3, lwd=2)
lines(density(probs.none[,5]-probs.none[,1], adjust=3), lwd=2)
abline(v=0, lty=2, col=grey(0.8))
plot(density(probs.9010[,2]-probs.9010[,1], adjust=3), col=grey(0.6), xlim=c(-.1,.5),
	xlab="change in predicted Pr('90/10') relative to unexposed control", ylab = "density",
	bty="n", las=1, lwd=2, main = "Most generous policy (90/10)")
lines(density(probs.9010[,3]-probs.9010[,1], adjust=3), lwd=2, lty=2, col=grey(0.6))
lines(density(probs.9010[,4]-probs.9010[,1], adjust=3), lty=3, lwd=2)
lines(density(probs.9010[,5]-probs.9010[,1], adjust=3), lwd=2)
abline(v=0, lty=2, col=grey(0.8))
legend(x="topright", 
	legend=c("information", "history", "Hungary",
	"exposed (cntrl)"),
	lty=c(1,2,3,1), col=c(grey(0.6),grey(0.6),"black","black"), bty="n",
	cex=.8, lwd=2)
dev.off()


### PS Models for Appendix; Table A-6
ps.basic.mult<-multinom(ps ~ treat + FXstatus, Hess = TRUE, weights = weight, data = obs.dat)

cov.names<-c("(Intercept)", "information", "history", "Hungary",
	"FX-exposed", "past borrower")
htmlreg(ps.basic.mult,
	type="html",
	file = "./tables_figs/SM/TabA6.html",
	custom.coef.names = cov.names,
	digits = 2,
	stars= 0,
	#caption.above = TRUE,
	#caption = "Preference for government intervention.  Weighted logistic regression over 20 imputed datasets.",
	caption = "",
	html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,
	include.aic = TRUE, include.deviance = FALSE,
    include.bic = FALSE, include.loglik = FALSE
    )


## voter behavior
### figure 5
vc.sumtab<-table(obs.dat$FXstatus,obs.dat$vote.intent.rev3)
vc.sumtab<-vc.sumtab[c(2,3,1),c(4,1,3,2)]
vc.proptab<-prop.table(vc.sumtab,1)
ci.vc<-NULL
for(i in 1:nrow(vc.sumtab)){ci.vc[[i]]<-multinomialCI(vc.sumtab[i,], alpha=0.05)}


xpts<-c(0,1,2,3)
barCenters<-rbind(xpts-.12, xpts, xpts+.12)
pdf("./tables_figs/Fig5.pdf", width = 8, height=6.3)
plot(barCenters[1,],vc.proptab[1,], 
	ylim=c(0,0.6), bty="n", xaxt = "n", cex=1.25, pch=16, 
	xlim=c(-.5,3.5), ylab="proportion of borrower group", xlab="", las=1)
axis(1,at=xpts, labels = c("PiS", "PO/PSL", "other", "abstain"))
points(barCenters[2,],vc.proptab[2,], cex=1.25, pch=17, col=grey(0.7))
points(barCenters[3,],vc.proptab[3,], cex=1.25, pch=18, col=grey(0.7))
arrows(barCenters[1,],ci.vc[[1]][,1], barCenters[1,], ci.vc[[1]][,2],
       lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
arrows(barCenters[2,],ci.vc[[2]][,1], barCenters[2,], ci.vc[[2]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
arrows(barCenters[3,],ci.vc[[3]][,1], barCenters[3,], ci.vc[[3]][,2],
       lwd = 1.5, angle = 90, col=grey(0.7),
       code = 3, length = 0.05)
legend(x="top", legend=c("exposed", "past borrower", "never borrowed"),
	pch=c(16,17,18), col=c("black",grey(0.7),grey(0.7)), lty=1, bty="n",
	cex=.7, horiz=TRUE)
dev.off()

## Vote Choice & turnout models
vc.simp<-vote.intent.rev3 ~ FXstatus + past.turnout+
		age.expand +  female + married + income.quint + ed_level + 
		urban_rural + employed + religion + 
		hh_size + LeftRightCat + province

vc.int<-vote.intent.rev3 ~ FXstatus*POPSL2011 + past.turnout+ 
		age.expand +  female + married + income.quint + ed_level + 
		urban_rural + employed + religion + 
		hh_size + LeftRightCat + province

vc.out<-b.vc.out<-se.vc.out<-vc.ni<-b.vc.ni<-se.vc.ni<-pp.out<-pp.ni<-NULL
for(i in 1:imp.out$m){
	vc.ni[[i]]<-multinom(vc.simp, 
		data = imp.out$imputations[[i]],
		weights=weight, Hess = TRUE, maxit = 200, model=TRUE)
	vc.out[[i]]<-multinom(vc.int, 
		data = imp.out$imputations[[i]],
		weights=weight, Hess = TRUE, maxit = 200, model=TRUE)
	b.vc.out<-rbind(b.vc.out,as.vector(summary(vc.out[[i]])$coefficients))
	se.vc.out<-rbind(se.vc.out,as.vector(summary(vc.out[[i]])$standard.errors))
	pp.out<-cbind(pp.out, fitted(vc.out[[i]])[,4])
	b.vc.ni<-rbind(b.vc.ni,as.vector(summary(vc.ni[[i]])$coefficients))
	se.vc.ni<-rbind(se.vc.ni,as.vector(summary(vc.ni[[i]])$standard.errors))
	pp.ni<-cbind(pp.ni, fitted(vc.ni[[i]])[,4])
}
vc.meld.ni <- mi.meld(q = b.vc.ni, se = se.vc.ni)
vc.meld <- mi.meld(q = b.vc.out, se = se.vc.out)

cov.names<-c("FX-exposed", "past FX borrower", "PO/PSL 2011", 
	"past turnout", "18-31", "44-56", "57-66", "66+", "female", "married", "income", "education",
	"urban", "employed", "religiosity", "household size", 
	"Left", "Right", "exposed x 2011 PO/PSL", "past x 2011 PO/PSL", "(Intercept)")


stargazer(vc.ni[[1]],vc.out[[1]],  #Table 2 & A-8
	type="html",
	coef= list(matrix(vc.meld.ni$q.mi, nrow=3), matrix(vc.meld$q.mi, nrow=3)),
	se = list(matrix(vc.meld.ni$se.mi, nrow=3),matrix(vc.meld$se.mi, nrow=3)),
	omit = "province",
	covariate.labels = cov.names,
	digits = 2,
	star.cutoffs = 0,
	nobs=TRUE,
	out = "./tables_figs/Tab2.html",
	table.layout = "#tncm-"
	)
#p.values for table 2/A-8 model 3
p.vals.t2.m3<-t(matrix(round(2*pnorm(abs(vc.meld.ni$q.mi/vc.meld.ni$se.mi), lower=F),2), nrow=3))
colnames(p.vals.t2.m3)<-c("abstain", "other", "PiS")
rownames(p.vals.t2.m3)<-vc.ni[[1]]$coefnames
#p.values for table 2/A-8 model 4
p.vals.t2.m4<-t(matrix(round(2*pnorm(abs(vc.meld$q.mi/vc.meld$se.mi), lower=F),2), nrow=3))
colnames(p.vals.t2.m4)<-c("abstain", "other", "PiS")
rownames(p.vals.t2.m4)<-vc.out[[1]]$coefnames

	
#first diff analysis
X.avg.po<-X.chf.po<-X.avg<-X.chf<-vc.out[[i]]$model[1,]
X.avg.po[1,]<-X.chf.po[1,]<-X.avg[1,]<-X.chf[1,]<-data.frame("PiS", 
	"none", 
	1, #past incumbent support
	1, #past turnout
	"[32,44)", #reference age group (near-modal)
	1, # female (median respondent)
	1, #married (median respondent)
	3, #median income quintile
	2, #median education level
	2, #median settlement size
	2, #median employment value
	4, #median religiosity
	3, #median household size
	"right", #modal political persuasion
	"Mazowieckie", #modal province
	1) #weight
X.chf.po[2]<-X.chf[2]<-"exposed"
X.chf[3]<-X.avg[3]<-0

pp.avg<-pp.chf<-pp.chf.po<-pp.avg.po<-NULL
for(i in 1:imp.out$m){
	pp.avg<-rbind(pp.avg, predict(vc.out[[i]], newdata=X.avg,type="probs"))
	pp.chf<-rbind(pp.chf, predict(vc.out[[i]], newdata=X.chf,type="probs"))
	pp.avg.po<-rbind(pp.avg.po, predict(vc.out[[i]], newdata=X.avg.po,type="probs"))
	pp.chf.po<-rbind(pp.chf.po, predict(vc.out[[i]], newdata=X.chf.po,type="probs"))

}
apply(pp.chf,2,mean) #exposed, not past incumbent supporter
apply(pp.avg,2,mean) #unexposed, not past incumbent supporter
apply(pp.chf.po,2,mean) #exposed, past incumbent supporter
apply(pp.avg.po,2,mean)#unexposed, past incumbent supporter

### Fig 6

X.pred<-model.matrix(vc.int, 
	data=rbind(X.avg,X.chf,X.avg.po,X.chf.po))
pp.mat.out<-NULL
pis.probs<-NULL
abs.probs<-NULL
l<-nrow(coef(vc.out[[1]]))
for(i in 1:imp.out$m){
	b<- as.vector(t(coef(vc.out[[i]]))) #vectorizing coef matrix to match vcov
	B.vc<-mvrnorm(500, b, vcov(vc.out[[i]])) #drawing 500 coef. vectors for each imputation
	for(j in 1:nrow(B.vc)){ #getting predicted PiS vote probability 
		B<-matrix(B.vc[j,], nrow = l, byrow=T)
		lp.cf<-B%*%t(X.pred)
		denom.cf<-1+colSums(exp(lp.cf))
		pp.mat.cf<-sweep(exp(lp.cf), 2, denom.cf, "/") #predicted probs
		pp.mat.cf<-rbind(pp.mat.cf, 1-colSums(pp.mat.cf))
		rownames(pp.mat.cf)<-c("NoVote", "other", "Pis", "PO/PSL")
		pis.probs<-rbind(pis.probs,pp.mat.cf[3,])
		abs.probs<-rbind(abs.probs,pp.mat.cf[1,])
	}
}

pdf("./tables_figs/Fig6.pdf", width = 11, height=6)
par(mfrow = c(1,2))
plot(density(pis.probs[,3], adjust=1.1), col=grey(0.6), xlim=c(0,1),
	xlab="predicted Pr(vote PiS)", ylab="density",
	main="", bty="n", las=1, lwd=2)
lines(density(pis.probs[,4], adjust = 1.1), lwd=2)
lines(density(pis.probs[,2], adjust = 1.1), lty=3, lwd=2)
lines(density(pis.probs[,1], adjust = 1.1), lty=3, lwd=2, col=grey(0.6))
legend(x="topright", 
	legend=c("unexposed, PO/PSL 2011", "exposed, PO/PSL 2011",
	"unexposed, non-PO/PSL 2011", "exposed, non-PO/PSL 2011"),
	lty=c(1,1,3,3), col=c(grey(0.6),"black"), bty="n",
	cex=.8, lwd=2)

plot(density(pis.probs[,2]-pis.probs[,1], adjust=1.1),lty=3, col=grey(0.5), xlim=c(-.5,.8),
	xlab="change in predicted Pr(vote PiS) due to FX exposure", ylab="density",
	main="", bty="n", las=1, lwd=2)
lines(density(pis.probs[,4]-pis.probs[,3], adjust = 1.1), lwd=2)
abline(v=0, 
	lty=2, col=grey(0.8))
legend(x="topright", legend=c("PO/PSL voter, 2011", "non-PO/PSL voter, 2011"),
	lty=c(1,3), col=c("black",grey(0.5),grey(0.7)), bty="n",
	cex=.8, lwd=2)
dev.off()


### Simulating election outcome
spv<-6  #midway between PiS and PO in table A-2.
maj.thresh<-230/spv/100  #vote share threshold for a majority in the Sejm
gap<-5/spv  #implied gap betw PiS vote share and majority threshold 
pis.vs.cf<-pis.seats<-pis.seats.cf<-NULL
n<-2044
k<-ncol(coef(vc.out[[1]]))
l<-nrow(coef(vc.out[[1]]))
for(i in 1:imp.out$m){
	X.actual<-X.cf<-model.matrix(vc.int, data=imp.out$imputations[[i]])
	X.cf[,grepl("FXstatusexposed",colnames(X.cf))]<-0
	b<- as.vector(t(coef(vc.out[[i]]))) #vectorizing coef matrix to match vcov
	B.vc<-mvrnorm(500, b, vcov(vc.out[[i]])) #drawing 500 coef. vectors for each imputation
	for(j in 1:nrow(B.vc)){ #getting predicted PiS vote share 
		B<-matrix(B.vc[j,], nrow = l, byrow=T)
		lp.cf<-B%*%t(X.cf)
		denom.cf<-1+colSums(exp(lp.cf))
		pp.mat.cf<-sweep(exp(lp.cf), 2, denom.cf, "/") #predicted probs
		pp.mat.cf<-rbind(pp.mat.cf, 1-colSums(pp.mat.cf))
		to.cf<-n-sum(pp.mat.cf[1,]*vc.out[[i]]$weights)
		pis.cf <- sum(pp.mat.cf[3,]*vc.out[[i]]$weights)
		pis.vs.cf<-c(pis.vs.cf, pis.cf/to.cf)
		pis.seats.cf<-c(pis.seats.cf, 100*pis.cf/to.cf*spv)
		lp<-B%*%t(X.actual)
		denom<-1+colSums(exp(lp))
		pp.mat<-sweep(exp(lp), 2, denom, "/") #predicted probs
		pp.mat<-rbind(pp.mat, 1-colSums(pp.mat))
		to<-n-sum(pp.mat[1,]*vc.out[[i]]$weights)
		pis<- sum(pp.mat[3,]*vc.out[[i]]$weights)
		pis.seats<-c(pis.seats, 100*pis/to*spv)#predicted PiS seats
	}
}


summary(PiS.vs.surv-pis.vs.cf)
summary(pis.seats) #to calibrate assumptions; 235 in actual election
pdf("./tables_figs/SM/FigA3.pdf", width = 8, height=6.3)	
plot(density(PiS.vs.surv-pis.vs.cf), lwd=2, bty="n",
	main="Estimated Consequences of CHF shock on PiS Vote Share", 
	xlab="Change in PiS vote share", las=1, xlim=c(-.05,.05))
abline(v=gap/100, lty=2, col=grey(0.2))
abline(v=0, lty=3, col=gray(0.8))
text(0.03,33,"majority\nthreshold")
arrows(0.02,33,.009,33, length = .1, angle = 15 )
dev.off()

#quantities mentioned in the main text
sum(PiS.vs.surv-pis.vs.cf>0)/length(pis.vs.cf)
sum(PiS.vs.surv-pis.vs.cf>gap/100)/length(pis.vs.cf)


### Sub policy for FX exposure for SM

vc.gi<-vote.intent.rev3 ~ gi.bin + past.turnout+
		age.expand +  female + married + income.quint + ed_level + 
		urban_rural + employed + religion + 
		hh_size + LeftRightCat + province  #model for full vc specification

vc.ps<-vote.intent.rev3 ~ ps + past.turnout+ 
		age.expand +  female + married + income.quint + ed_level + 
		urban_rural + employed + religion + 
		hh_size + LeftRightCat + province  #model for full vc specification

out.gi<-b.vc.gi<-se.vc.gi<-out.ps<-b.vc.ps<-se.vc.ps<-pp.gi<-pp.ps<-NULL
for(i in 1:imp.out$m){
	out.gi[[i]]<-multinom(vc.gi, 
		data = imp.out$imputations[[i]],
		weights=weight, Hess = TRUE, maxit = 200, model=TRUE)
	out.ps[[i]]<-multinom(vc.ps, 
		data = imp.out$imputations[[i]],
		weights=weight, Hess = TRUE, maxit = 200, model=TRUE)
	b.vc.ps<-rbind(b.vc.ps,as.vector(summary(out.ps[[i]])$coefficients))
	se.vc.ps<-rbind(se.vc.ps,as.vector(summary(out.ps[[i]])$standard.errors))
	pp.ps<-cbind(pp.ps, fitted(out.ps[[i]])[,4])
	b.vc.gi<-rbind(b.vc.gi,as.vector(summary(out.gi[[i]])$coefficients))
	se.vc.gi<-rbind(se.vc.gi,as.vector(summary(out.gi[[i]])$standard.errors))
	pp.gi<-cbind(pp.gi, fitted(out.gi[[i]])[,4])
}
vc.meld.gi <- mi.meld(q = b.vc.gi, se = se.vc.gi)
vc.meld.ps <- mi.meld(q = b.vc.ps, se = se.vc.ps)

cov.names<-c("Support intervention", "DK", "Some", "50/50", "90/10", 
	"past turnout", "18-31", "44-56", "57-66", "66+", "female", "married", "income", "education",
	"urban", "employed", "religiosity", "household size", 
	"Left", "Right", "(Intercept)")


stargazer(out.gi[[1]],out.ps[[1]],
	type="html",
	coef= list(matrix(vc.meld.gi$q.mi, nrow=3), matrix(vc.meld.ps$q.mi, nrow=3)),
	se = list(matrix(vc.meld.gi$se.mi, nrow=3),matrix(vc.meld.ps$se.mi, nrow=3)),
	omit = "province",
	covariate.labels = cov.names,
	digits = 2,
	star.cutoffs = 0,
	nobs=TRUE,
	out = "./tables_figs/SM/TabA9.html",
	table.layout = "#tncm-"
	)

# Matching for SM
test.d<-NULL
for(i in 1:imp.out$m){
test.d[[i]]<-subset(imp.out$imputations[[i]], 
	select = c("age.expand", "married",
	"income.quint", "ed_level", "urban_rural", "employed", "hh_size", "gi.bin",
	"ps", "vote.intent.rev3", "exposed", "FXstatus")
	)
}

obs.d<-subset(obs.dat, 
	select = c("age.expand", "married",
	"income.quint", "ed_level", "urban_rural", "employed", "hh_size", "gi.bin",
	"ps", "vote.intent.rev3", "exposed", "FXstatus")
	)
m1<-cem("exposed", datalist= test.d, data=obs.d,
	drop=c("gi.bin","ps","FXstatus", "vote.intent.rev3"))
gi.m1<-att(m1, gi.bin~exposed, test.d, model="logit")
ps9010.m1<-att(m1, ps=="90/10"~exposed, test.d, model="logit")
psnone.m1<-att(m1, ps=="none"~exposed, test.d, model="logit")
vcpis.m1<-att(m1, vote.intent.rev3=="PiS" ~exposed, test.d, model="logit")
vcabstain.m1<-att(m1, vote.intent.rev3=="NoVote" ~exposed, test.d, model="logit")

m2<-cem("exposed", datalist= test.d,
	drop=c("gi.bin","ps","FXstatus", "vote.intent.rev3"))
gi.m2<-att(m2, gi.bin~exposed, test.d, model="logit")
ps9010.m2<-att(m2, ps=="90/10"~exposed, test.d, model="logit")
psnone.m2<-att(m2, ps=="none"~exposed, test.d, model="logit")
vcpis.m2<-att(m2, vote.intent.rev3=="PiS" ~exposed, test.d, model="logit")
vcabstain.m2<-att(m2, vote.intent.rev3=="NoVote" ~exposed, test.d, model="logit")
DV.s<-c("Gov't intervention", "90/10", "None", "PiS", "Abstain")
ests.1<-c(
	paste(round(gi.m1$att.model[1,2],2), " (", round(gi.m1$att.model[2,2],2),")", sep=""),
	paste(round(ps9010.m1$att.model[1,2],2), " (", round(ps9010.m1$att.model[2,2],2),")", sep=""),
	paste(round(psnone.m1$att.model[1,2],2), " (", round(psnone.m1$att.model[2,2],2),")", sep=""),
	paste(round(vcpis.m1$att.model[1,2],2), " (", round(vcpis.m1$att.model[2,2],2),")", sep=""),
	paste(round(vcabstain.m1$att.model[1,2],2), " (", round(vcabstain.m1$att.model[2,2],2),")", sep="")
	)
ests.2<-c(
	paste(round(gi.m2$att.model[1,2],2), " (", round(gi.m2$att.model[2,2],2),")", sep=""),
	paste(round(ps9010.m2$att.model[1,2],2), " (", round(ps9010.m2$att.model[2,2],2),")", sep=""),
	paste(round(psnone.m2$att.model[1,2],2), " (", round(psnone.m2$att.model[2,2],2),")", sep=""),
	paste(round(vcpis.m2$att.model[1,2],2), " (", round(vcpis.m2$att.model[2,2],2),")", sep=""),
	paste(round(vcabstain.m2$att.model[1,2],2), " (", round(vcabstain.m2$att.model[2,2],2),")", sep="")
	)
match.out<-data.frame(ests.1, ests.2)
rownames(match.out)<-DV.s
stargazer(match.out,type="html", summary=F,
	out = "./tables_figs/SM/TabA10.html")

#immigration stuff for SM
##anti-migrant opinion plots for appendix.
pdf("./tables_figs/SM/FigA2.pdf", width = 11, height=6)
par(mfrow=c(1,2))
vioplot(na.omit(obs.dat$antimigrant.summary[obs.dat$gi=="DK" & obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$gi=="none"& obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$gi=="some"& obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$gi=="big"& obs.dat$exposed==F]),
	names = c("DK", "none", "some", "big"),
	col=grey(0.8))
title(main = "Anti-immigrant sentiment & intervention support\n among unexposed", ylab="anti-immigrant sentiment")
abline(h=median(na.omit(obs.dat$antimigrant.summary[obs.dat$gi=="big"& obs.dat$exposed==F])), lty=2)


vioplot(na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="DK" & obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="none"& obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="some"& obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="50/50"& obs.dat$exposed==F]),
	na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="90/10"& obs.dat$exposed==F]),
	names = c("DK", "none", "some", "50/50", "90/10"),
	col=grey(0.8))
title(main = "Anti-immigrant sentiment & proposal support\n among unexposed", ylab="anti-immigrant sentiment")
abline(h=median(na.omit(obs.dat$antimigrant.summary[obs.dat$ps=="90/10"& obs.dat$exposed==F])), lty=2)
dev.off()

im.gi<-glm(gi.bin ~ treat + exposed + antimigrant.summary,
		family=quasibinomial,
		weight = weight, 
		data = obs.dat)
im.ps<-multinom(ps~treat+ exposed + antimigrant.summary  , weight = weight, 
		data = obs.dat)

htmlreg(im.gi, file="./tables_figs/SM/TabA7-L.html") #tab A-7 left
htmlreg(im.ps, file="./tables_figs/SM/TabA7-R.html") #tab A-7 right
#END