####
# This code produces:
# AJPSimputations.Rdata, AJPScleanedData.Rdata, AJPScleanedData.csv & codebook
# AJPSimputations.Rdata includes the 20 imputed datasets   
# used in the analysis appearing in the
# manuscript and supplemental materials
# "The Political Consequences of External Economic Shocks"
# Ahlquist, Copelovitch, & Walter. American Journal of Political Science
# 
# last updated: 20 October 2019
# R version 3.3.1 (2016-06-21)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.13.6 (unknown)
# Imputation uses Amelia II for R version 1.7.4
# other attached packages:
# Hmisc_4.1-1
# ggplot2_3.1.0   
# Formula_1.2-2   
# survival_2.41-3 
# lattice_0.20-35 
# Rcpp_1.0.1 
# foreign_0.8-69  
# MASS_7.3-51.4    
#####


rm(list=ls())
gc()
library(MASS)
library(foreign)
library(Amelia)
library(Hmisc)

rng<-5879521
set.seed(rng)

#setwd() set the appropriate working directory
load("Oct2015CBOS.Rdata")

#cleaning and recoding data
octdat$income.quint<-NA
octdat$income.quint[octdat$monthly_incPC=="no income"]<-0
octdat$income.quint[octdat$monthly_incPC=="Up to PLN 649"]<-1
octdat$income.quint[octdat$monthly_incPC=="From PLN 650 to 999"]<-2
octdat$income.quint[octdat$monthly_incPC=="From PLN 1,000 to 1,399"]<-3
octdat$income.quint[octdat$monthly_incPC=="From PLN 1,400 to 1,999"]<-4
octdat$income.quint[octdat$monthly_incPC=="2,000 PLN and more"]<-5
octdat$age<-2015-octdat$birth_year
octdat$employed<-as.numeric(octdat$employed)
octdat$past.turnout<-as.numeric(octdat$past.turnout)-1
octdat$currentFXloan<-octdat$FXloan
octdat$currentFXloan[octdat$repayFXloan=="No"]<-"No"
octdat$currentCHFloan<-"No"
octdat$currentCHFloan[octdat$loan_CHF=="Swiss franc"]<-"Yes"
octdat$currentCHFloan[is.na(octdat$FXloan)]<-NA
octdat$currentCHFloan<-as.factor(octdat$currentCHFloan)
octdat$pastFXloan<-octdat$FXloan
octdat$pastFXloan[octdat$currentFXloan=="Yes"]<-"No"
octdat$LeftRight<-octdat$political_views
octdat$LeftRight[octdat$LeftRight==97]<-NA
octdat$LeftRight<-octdat$LeftRight-4 #0 is midpoint (modal response)
octdat$female<-as.numeric(octdat$sex==2)
octdat$retired<-as.numeric(octdat$profession==13)
octdat$in.labor.force<-as.numeric(octdat$profession==14 | octdat$profession<12)
octdat$mean_income<- octdat$mean_income/100 #to make closer to other values for imputation
octdat$PiS2011<-as.numeric(octdat$past_vote==1)
octdat$PiS2011[octdat$past_vote==97]<-NA
octdat$PiS2011[octdat$past_election==9]<-0 #nonvoters in last election
octdat$PiS2011[octdat$past_election==2]<-0 #nonvoters in last election
octdat$POPSL2011<-as.numeric(octdat$past_vote==7 | octdat$past_vote==5)
octdat$POPSL2011[octdat$past_vote==97]<-NA
octdat$POPSL2011[octdat$past_election==9]<-0 #nonvoters in last election
octdat$POPSL2011[octdat$past_election==2]<-0 #nonvoters in last election

octdat$vote.intent.rev<-"other"
octdat$vote.intent.rev[is.na(octdat$parliament_election)]<-NA
octdat$vote.intent.rev[grep("certainly not",octdat$parliament_election)]<-"NoVote"
octdat$vote.intent.rev[grep("whether",octdat$parliament_election)]<-"DK"
octdat$vote.intent.rev[octdat$parliament_vote=="Don’t know/not sure"]<-"DK"
octdat$vote.intent.rev[octdat$parliament_vote=="PiS"]<-"PiS"
octdat$vote.intent.rev[octdat$parliament_vote=="po"]<-"PO/PSL"
octdat$vote.intent.rev[octdat$parliament_vote=="psl"]<-"PO/PSL"
octdat$vote.intent.rev<-as.factor(octdat$vote.intent.rev)

#vote share from october survey; referenced
sum(octdat[octdat$vote.intent=="PORP","weight"], na.rm=T)/sum(octdat[!is.na(octdat$vote.intent),"weight"], na.rm=T)
sum(octdat[octdat$vote.intent=="PiS","weight"], na.rm=T)/sum(octdat[!is.na(octdat$vote.intent),"weight"], na.rm=T)

octdat$ps<-octdat$gi<-NA  #shortened versions of policy opinion variables
octdat$gov.int.bin<-as.numeric(octdat$gov_intervene %in% c("Big intervention", "Some intervention"))
octdat$gov.int.bin[is.na(octdat$gov_intervene)]<-NA
octdat$gi[octdat$gov_intervene=="Big intervention"]<-"big"
octdat$gi[octdat$gov_intervene=="Some intervention"]<-"some"
octdat$gi[octdat$gov_intervene=="Do not intervene"]<-"none"
octdat$gi[octdat$gov_intervene=="Not sure/don’t know"]<-"DK"
octdat$gi<-as.factor(octdat$gi)
octdat$ps[octdat$proposal_support=="Proposal A, where the cost is equally split between banks and borrowers"]<-"50/50"
octdat$ps[octdat$proposal_support=="Proposal B, where banks pay 90% and borrowers 10% of the costs"]<-"90/10"
octdat$ps[octdat$proposal_support=="The government should do nothing, meaning the mortgage borrowers bear all the costs."]<-"none"
octdat$ps[octdat$proposal_support=="The government should something but I do not support either Proposal A or Proposal B"]<-"some"
octdat$ps[octdat$proposal_support==" Don’t know"]<-"DK"
octdat$ps<-as.factor(octdat$ps)

# Other opinion variables used in imputation model
octdat$country_direction[octdat$country_direction==7]<-NA
octdat$country_change[octdat$country_change==7]<-NA
octdat$economic_situation[octdat$economic_situation==7]<-NA
octdat$political_situation[octdat$political_situation==7]<-NA
octdat$economy_change[octdat$economy_change==7]<-NA
octdat$attitude_Kopacz[octdat$attitude_Kopacz==7]<-NA
octdat$attitude_Kopacz[octdat$attitude_Kopacz==5]<-2.5 #neutral in middle
octdat$politics_interest[octdat$politics_interest==6]<-NA
octdat$policies_improve_economic[octdat$policies_improve_economic==7]<-NA

# FX loan encoding
octdat$FXloan<-as.numeric(octdat$FXloan)-1
octdat$currentFXloan <- as.numeric(octdat$currentFXloan)-1
octdat$currentCHFloan <- as.numeric(octdat$currentCHFloan)-1
octdat$pastFXloan<- as.numeric(octdat$pastFXloan)-1
octdat$know_FXborrower<- as.numeric(octdat$know_FXborrower)-1

# turnout encoding
octdat$turnout.intent.ord<-NA
octdat$turnout.intent.ord[grep("certainly not",octdat$parliament_election)]<-0
octdat$turnout.intent.ord[grep("certainly take",octdat$parliament_election)]<-2
octdat$turnout.intent.ord[grep("whether",octdat$parliament_election)]<-1

# immigration variables not used for imputation
octdat$immigration.summary<-apply( octdat[,grep("immigration",colnames(octdat))],
	MARGIN = 1, FUN= mean, na.rm=T) #summary measure with bigger numbers as more tolerant of migrants
octdat$antimigrant.summary<- 7 - octdat$immigration.summary  #reversing immigration variable; bigger is more antimigrant

# variables for imputation model 
impdat<-octdat[,c("respondent", "female", "age", "retired", 
	"in.labor.force", "employed", "hh_size", "adults_hh", "ed_level",
	"country_direction", 
	"country_change",
	"economic_situation",
	"political_situation", 
	"economy_change", 
	"attitude_Kopacz",
	"hh_finances", "future_finances", 
	"politics_interest", 
	"PiS2011","POPSL2011", "religious_participation", 
	"DATE", 
	"weight", 
	"married","urban_rural", "income.quint",
	"policies_improve_economic",
	"know_FXborrower", "treat",
	"currentFXloan", 
	"pastFXloan",   
	"province", 
	"turnout.intent.ord", 
	"vote.intent.rev",
	"past.turnout",
	"LeftRight", 
	"ps", "gi"
	)] 


#imputation
bound.mat<-rbind(
	c(which(names(impdat)=="POPSL2011"),0,1),
	c(which(names(impdat)=="PiS2011"),0,1),
	c(which(names(impdat)=="income.quint"),0,5),
	c(which(names(impdat)=="know_FXborrower"),0,1),
	c(which(names(impdat)=="currentFXloan"),0,1),
	c(which(names(impdat)=="pastFXloan"),0,1),
	c(which(names(impdat)=="turnout.intent.ord"),0,2),
	c(which(names(impdat)=="religious_participation"),1,5),
	c(which(names(impdat)=="past.turnout"),0,1),
	c(which(names(impdat)=="LeftRight"),-3,3)
	)

imp.out<-amelia(impdat, m=20, 
	noms = c("vote.intent.rev","treat","ps", "gi"),
	idvars = c("respondent", "weight", "DATE","province"),
	bounds = bound.mat
	)
oi.mi<- overimpute(imp.out, var = "income.quint")


for(i in 1:length(imp.out$imputations)){
	imp.out$imputations[[i]]$gi.bin<-as.numeric(imp.out$imputations[[i]]$gi %in% c("big","some"))
	imp.out$imputations[[i]]$LeftRightCat<-"center"
	imp.out$imputations[[i]]$LeftRightCat[imp.out$imputations[[i]]$LeftRight<0]<-"left"
	imp.out$imputations[[i]]$LeftRightCat[imp.out$imputations[[i]]$LeftRight>0]<-"right"
	imp.out$imputations[[i]]$LeftRightCat<-as.factor(imp.out$imputations[[i]]$LeftRightCat)
	imp.out$imputations[[i]]$ps<-relevel(imp.out$imputations[[i]]$ps, ref="none")
	imp.out$imputations[[i]]$vote.intent.rev<-relevel(imp.out$imputations[[i]]$vote.intent.rev, ref="PO/PSL")
	imp.out$imputations[[i]]$age.expand<-as.factor(cut2(imp.out$imputations[[i]]$age, g=5))
	imp.out$imputations[[i]]$age.expand<-relevel(imp.out$imputations[[i]]$age.expand, ref="[32,44)")
}

#writes each imputed dataset to a separate file if desired
#write.amelia(imp.out, separate = TRUE, file.stem= "AJPSImputations") 

vars<-c(names(imp.out$imputations[[1]]), "immigration.summary", "antimigrant.summary")
save(imp.out,file="AJPSimputations.Rdata")
octdat<-octdat[,names(octdat)%in% vars]
save(octdat, file = "AJPScleanedData.Rdata")
write.table(octdat, file = "AJPScleanedData.csv", sep=",")
##END