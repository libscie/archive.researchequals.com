#############################################################################
# Individual patient data meta-analysis Type D personality & Adverse events #
#                                                                           #
# Script written by: Paul Lodder                                             #
# Version: October 16th 2020                                                #
#                                                                           #
##############################################################################

#### Set working directory and load libraries ####
setwd("~/My Drive/Work/Research/PhD projects/Project7_IPDmeta-analysis/Lodder et al. (2021) IPDMA datapackage/IPD meta-analysis")
library(xlsx)
library(metafor)
library(lme4)
library(dplyr)
library(psych)
library(ggplot2)
library(gridExtra)
library(brms)
library(rstan)
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(tidybayes)
library(ggpubr)
library(metafor)


#### Read data ####
rm(list=ls())
dat<-read.csv(file="IPDMA_171.csv",header=T,sep=";",dec=",")
dat<-read.xlsx(file="IPDMA_18.xlsx",header=T,sheetIndex="IPDMA_17 2")
dat$AGEold<-dat$AGE
dat$AGE<-scale(as.numeric(as.character(dat$AGE)))
dat$AGE2<-dat$AGE*dat$AGE
dat$Dataset<-factor(dat$Dataset)
#dat<-read.xlsx(file="IPDMA_5.xlsx",sheetIndex = 1,header=T)
head(dat)
names(dat)
dim(dat)

#### Create numeric study ID and label variable ####
didtable<-data.frame(code=levels(dat$Dataset),
                     year=c(2011,2016,2011,2014,2015,2015,2016,2017,2017,2019,2011,1996,2013,2013,2000,2010,2020,2010,2010),
                     label=factor(c("Coyne et al. (2011)","Herrmann-Lingen et al. (2016)","Grande et al. (2011)","Meyer et al. (2014)","Sumin et al. (2015)",
                             "Dulfer et al. (2015)","Gostoli et al. (2016)","Pushkarev et al. (2017)","Conden et al. (2017)","Lin et al. (2019)",
                             "Schmidt et al. (2011)","Denollet et al. (1996)","Denollet et al. (2013a)","Denollet et al. (2013b)","Denollet et al. (2000)",
                             "Denollet et al. (2006)","Lv et al. (2020)","Martens et al. (2010)","Pelle et al. (2010)")
                     ))
didtable<-didtable[order(didtable$year),]
didtable<-as.data.frame(cbind(DID=1:dim(didtable)[1],didtable))

dat$DID<-NA
dat$Dlabel<-NA
for(i in 1:dim(dat)[1]){
  dat$Dlabel[i]<-as.character(didtable[which(didtable$code==dat$Dataset[i]),4])
  dat$Dyear[i]<-didtable[which(didtable$code==dat$Dataset[i]),3]
  dat$DID[i]<-didtable[which(didtable$code==dat$Dataset[i]),1]
}
dat$Dlabel<-factor(dat$Dlabel)
dat<-dat[order(dat$Dyear),]


### Compute MACE
dat$OUT_AE<-ifelse(is.na(dat$OUT_AE) & dat$OUT_ACM==1,1,dat$OUT_AE)
dat$OUT_AE<-ifelse(is.na(dat$OUT_AE) & dat$OUT_MI==1,1,dat$OUT_AE)
dat$OUT_AE<-ifelse(is.na(dat$OUT_AE) & dat$OUT_CABG==1,1,dat$OUT_AE)
dat$OUT_AE<-ifelse(is.na(dat$OUT_AE) & dat$OUT_PCI==1,1,dat$OUT_AE)


### Construct DS14 proxies based on other scales
sdna<-sd(dat$NASO[which(dat$Dataset=="Denollet_3")],na.rm=T)
sdsi<-sd(dat$SISO[which(dat$Dataset=="Denollet_3")],na.rm=T)
mna<-mean(dat$NASO[which(dat$Dataset=="Denollet_3")],na.rm=T)
msi<-mean(dat$SISO[which(dat$Dataset=="Denollet_3")],na.rm=T)

# Transform DS proxies to DS14 scale
dat$NASO[which(dat$Dataset=="Denollet_1" & !is.na(dat$Naproxy))]<-scale(dat$Naproxy[which(dat$Dataset=="Denollet_1" & !is.na(dat$Naproxy))])*sdna+mna
dat$SISO[which(dat$Dataset=="Denollet_1" & !is.na(dat$Siproxy))]<-scale(dat$Siproxy[which(dat$Dataset=="Denollet_1" & !is.na(dat$Siproxy))])*sdsi+msi

# Transform DS16 to DS14 scale
dat$NASO[which(dat$Dataset=="Denollet_1" & !is.na(dat$NA16))]<-scale(dat$NA16[which(dat$Dataset=="Denollet_1" & !is.na(dat$NA16))])*sdna+mna
dat$SISO[which(dat$Dataset=="Denollet_1" & !is.na(dat$SI16))]<-scale(dat$SI16[which(dat$Dataset=="Denollet_1" & !is.na(dat$SI16))])*sdsi+msi
dat$NASO[which(dat$Dataset=="Denollet_2" & !is.na(dat$NA16))]<-scale(dat$NA16[which(dat$Dataset=="Denollet_2" & !is.na(dat$NA16))])*sdna+mna
dat$SISO[which(dat$Dataset=="Denollet_2" & !is.na(dat$SI16))]<-scale(dat$SI16[which(dat$Dataset=="Denollet_2" & !is.na(dat$SI16))])*sdsi+msi
dat$NASO[which(dat$Dataset=="Denollet_3" & !is.na(dat$NA16))]<-scale(dat$NA16[which(dat$Dataset=="Denollet_3" & !is.na(dat$NA16))])*sdna+mna
dat$SISO[which(dat$Dataset=="Denollet_3" & !is.na(dat$SI16))]<-scale(dat$SI16[which(dat$Dataset=="Denollet_3" & !is.na(dat$SI16))])*sdsi+msi



#### Calculate TYPE D effect variables ####

## Standardize the NA and SI scores within each study ####
dat$NAst <- ave(dat$NASO, dat$Dataset, FUN=function(x) scale(x)) 
dat$SIst <- ave(dat$SISO, dat$Dataset, FUN=function(x) scale(x)) 
dat[which(dat$Dataset %in% c("Denollet_1","Denollet_2","Denollet_3","Denollet_4")),c("NAst","SIst")]<-dat[which(dat$Dataset %in% c("Denollet_1","Denollet_2","Denollet_3","Denollet_4")),c("NAZ","SIZ")]

## Calculate NA*SI interaction and quadratic effects ####
dat$NA2<-dat$NAst*dat$NAst
dat$SI2<-dat$SIst*dat$SIst
dat$NASI<-dat$NAst*dat$SIst

## Calculate 4-group method variable, including dummies for regression analysis
dat$TYPED4<-rep(NA,dim(dat)[1])
dat$TYPED4[which(dat$NASO>=10 & dat$SISO>=10)]<-0
dat$TYPED4[which(dat$NASO>=10 & dat$SISO<10)]<-1
dat$TYPED4[which(dat$NASO<10 & dat$SISO>=10)]<-2
dat$TYPED4[which(dat$NASO<10 & dat$SISO<10)]<-3
dat$TYPED4<-factor(dat$TYPED4,levels=0:3,labels=c("NA+SI+ (Type D)","NA+SI-","NA-SI+","NA-SI-"))
dat$HighNA<-rep(NA,dim(dat)[1])
dat$HighNA[which(dat$NASO>=10 & dat$SISO<10)]<-1
dat$HighNA[which(dat$NASO>=10 & dat$SISO>=10)]<-0
dat$HighNA[which(dat$NASO<10 & dat$SISO<10)]<-0
dat$HighNA[which(dat$NASO<10 & dat$SISO>=10)]<-0
dat$HighSI<-rep(NA,dim(dat)[1])
dat$HighSI[which(dat$NASO<10 & dat$SISO>=10)]<-1
dat$HighSI[which(dat$NASO>=10 & dat$SISO>=10)]<-0
dat$HighSI[which(dat$NASO<10 & dat$SISO<10)]<-0
dat$HighSI[which(dat$NASO>=10 & dat$SISO<10)]<-0
dat$Ref<-rep(NA,dim(dat)[1])
dat$Ref[which(dat$NASO<10 & dat$SISO>=10)]<-0
dat$Ref[which(dat$NASO>=10 & dat$SISO>=10)]<-0
dat$Ref[which(dat$NASO<10 & dat$SISO<10)]<-1
dat$Ref[which(dat$NASO>=10 & dat$SISO<10)]<-0
dat$TYPED2<-rep(NA,dim(dat)[1])
dat$TYPED2[which(dat$NASO<10 & dat$SISO>=10)]<-0
dat$TYPED2[which(dat$NASO>=10 & dat$SISO>=10)]<-1
dat$TYPED2[which(dat$NASO<10 & dat$SISO<10)]<-0
dat$TYPED2[which(dat$NASO>=10 & dat$SISO<10)]<-0

# Create dichotomous CAD vs HF measure
dats<-dat[which(dat$DISEASE %in% c("CAD","HF")),]
dats$DISEASEd<-ifelse(dats$DISEASE=="CAD",1,0)

#### Descriptives per study ####
dat %>% 
  group_by(Dataset) %>% 
  dplyr::summarize(N=n(),
            Age=mean(AGE,na.rm=T),
            Male=100*sum(SEX,na.rm=T)/n(),
            TypeD=100*sum(TYPED,na.rm=T)/n(),
            SI_14 = mean(SISO,na.rm=T),
            NA_14 = mean(NASO,na.rm=T),
            SI_16 = mean(SI16,na.rm=T),
            NA_16 = mean(NA16,na.rm=T),
            NAst = mean(NAst,na.rm=T),
            SIst = mean(SIst,na.rm=T),
            ACM_n = sum(OUT_ACM,na.rm=T),
            CM_n = sum(OUT_CM,na.rm=T),
            MI_n = sum(OUT_MI,na.rm=T),
            MACE_n = sum(OUT_MACE,na.rm=T),
            'ACM_%' = 100*sum(OUT_ACM,na.rm=T)/n(),
            'CM_%' = 100*sum(OUT_CM,na.rm=T)/n(),
            'MI_%' = 100*sum(OUT_MI,na.rm=T)/n(),
            'MACE_%' = 100*sum(OUT_MACE,na.rm=T)/n(),
            FU_years = mean(FOLLOWUP_YEARS,na.rm=T))->table1
psych::describe(dat)

# Correlation between NA & SI scores 
dat %>% 
  group_by(Dataset) %>% 
  dplyr::summarize(rnasi=cor(NAst,SIst, use="complete.obs"),
                   nn=n(),
                   mna=round(mean(NASO,na.rm=T),1),
                   sdna=round(sd(NASO,na.rm=T),1),
                   msi=round(mean(SISO,na.rm=T),1),
                   sdsi=round(sd(SISO,na.rm=T),1),
                   mna16=round(mean(NA16,na.rm=T),1),
                   sdna16=round(sd(NA16,na.rm=T),1),
                   msi16=round(mean(SI16,na.rm=T),1),
                   sdsi16=round(sd(SI16,na.rm=T),1))




##################################
# Make figures with descriptives #
##################################


# Scatterplot NA & SI
plotlist=list()
for(i in 1:19){
  temp<-subset(dat,dat$DID==i)
  if(i %in% c(1,5,9,13)){
    plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
      geom_count()+
      theme_minimal(base_size=13)+
      xlim(0,28)+
      ylim(0,28)+
      theme(legend.position="none")+
      xlab("")+ylab("SI")+ggtitle(temp$Dlabel[1])
  } else {
    if(i %in% c(18,19)){
      plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
        geom_count()+
        xlim(0,28)+
        ylim(0,28)+
        theme_minimal(base_size=13)+
        theme(legend.position="none")+
        xlab("NA")+ylab("")+ggtitle(temp$Dlabel[1])
    } else {
      if(i %in% c(17)){
        plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
          geom_count()+
          xlim(0,28)+
          ylim(0,28)+
          theme_minimal(base_size=13)+
          theme(legend.position="none")+
          xlab("NA")+ylab("SI")+ggtitle(temp$Dlabel[1])
      } else {
        
        plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
          geom_count()+
          xlim(0,28)+
          ylim(0,28)+
          theme_minimal(base_size=13)+
          theme(legend.position="none")+
          xlab("")+ylab("")+ggtitle(temp$Dlabel[1])
      }
      
    }
    
  }
  
}

pdf(file="ipdma_scats.pdf",width=15,height=15)
grid.arrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],plotlist[[5]],
             plotlist[[6]],plotlist[[7]],plotlist[[8]],plotlist[[9]],plotlist[[10]],
             plotlist[[11]],plotlist[[12]],plotlist[[13]],plotlist[[14]],plotlist[[15]],
             plotlist[[16]],plotlist[[17]],plotlist[[18]],plotlist[[19]],ncol=4,nrow=5)
dev.off()


#######################
### BAYESIAN MODELS ###
#######################
   
# Subsets of CAD or CHF
#dat<-dat[which(dat$DISEASE=="CAD"),]
#dat<-dat[which(dat$DISEASE=="HF"),]

# Set priors regression coefficient to normal(0,2)
priors0<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst  + (1 + AGE + SEX + NAst + SIst  || DID), data = dat)
priors0[2:6,1]<-"normal(0,2)"
priors0<-priors0[-(dim(priors0)[1]),]
priors1<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat)
priors1[2:8,1]<-"normal(0,2)"
priors1<-priors1[-(dim(priors1)[1]),]
priors2<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors2[2:9,1]<-"normal(0,2)"
priors2<-priors2[-(dim(priors2)[1]),]
priors3<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat)
priors3[2:5,1]<-"normal(0,2)"
priors3<-priors3[-(dim(priors3)[1]),]
priors4<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat)
priors4[2:7,1]<-"normal(0,2)"
priors4<-priors4[-(dim(priors4)[1]),]
priors5<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors5[2:14,1]<-"normal(0,2)"
priors5<-priors5[-(dim(priors5)[1]),]
priors6<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors6[2:14,1]<-"normal(0,2)"
priors6<-priors6[-(dim(priors6)[1]),]
priors7<-get_prior(OUT_ACM ~  AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat)
priors7[2:7,1]<-"normal(0,2)"
priors7<-priors7[-(dim(priors7)[1]),]
priors8<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors8[2:10,1]<-"normal(0,2)"
priors8<-priors2[-(dim(priors2)[1]),]
priors9<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats)
priors9[2:15,1]<-"normal(0,2)"
priors9<-priors9[-(dim(priors9)[1]),]

##############################

#### ACM ####
bacm0 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm1 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm2 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm3 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm4 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm5 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm6 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm7 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm8 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm9 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_acm<-list(bacm0,bacm1,bacm2,bacm3,bacm4,bacm5,bacm6,bacm7,bacm8)
save(res_acm,file="res_acm.rdata")
   
#### CM ####
bcm0 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm1 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm2 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm3 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm4 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm5 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm6 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)
bcm7 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm8 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm9 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_cm<-list(bcm0,bcm1,bcm2,bcm3,bcm4,bcm5,bcm6,bcm7,bcm8)
save(res_cm,file="res_cm.rdata")

#### MI ####
bmi0 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi1 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi2 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi3 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi4 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi5 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi6 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)
bmi7 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi8 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi9 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_mi<-list(bmi0,bmi1,bmi2,bmi3,bmi4,bmi5,bmi6,bmi7,bmi8)
save(res_mi,file="res_mi.rdata")

#### MACE ####
bmace0 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace1 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace2 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace3 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace4 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace5 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace6 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)
bmace7 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace8 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace9 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_mace<-list(bmace0,bmace1,bmace2,bmace3,bmace4,bmace5,bmace6,bmace7,bmace8)
save(res_mace,file="res_mace.rdata")

#### AE ####
bae0 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae1 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae2 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae3 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae4 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae5 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae6 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae7 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae8 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bae9 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 

res_ae<-list(bae0,bae1,bae2,bae3,bae4,bae5,bae6,bae7,bae8)
save(res_ae,file="res_ae.rdata")

#### CABG ####
bcabg0 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg1 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg2 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg3 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg4 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg5 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg6 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg7 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg8 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcabg9 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_cabg<-list(bcabg0,bcabg1,bcabg2,bcabg3,bcabg4,bcabg5,bcabg6,bcabg7,bcabg8)
save(res_cabg,file="res_cabg.rdata")

#### PCI ####
bpci0 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci1 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci2 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci3 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci4 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci5 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci6 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci7 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci8 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + AGE2 + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors8,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci9 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + DISEASEd + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*DISEASEd + SIst*DISEASEd + NA2*DISEASEd + SI2*DISEASEd + NASI*DISEASEd + (1 + AGE + SEX + DISEASEd + NAst + SIst + NA2 + SI2 + NASI || DID), data = dats, prior=priors9,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 

res_pci<-list(bpci0,bpci1,bpci2,bpci3,bpci4,bpci5,bpci6,bpci7,bpci8)
save(res_pci,file="res_pci.rdata")


# Predicted vs observed values
two.p1=predict(bacm3)[,1]
two.o1=bacm3$data$OUT_ACM
four.p1=predict(bacm4)[,1]
four.o1=bacm4$data$OUT_ACM
cont.p1=predict(bacm7)[,1]
cont.o1=bacm7$data$OUT_ACM

two.p2=predict(bcm3)[,1]
two.o2=bcm3$data$OUT_CM
four.p2=predict(bcm4)[,1]
four.o2=bcm4$data$OUT_CM
cont.p2=predict(bcm2)[,1]
cont.o2=bcm2$data$OUT_CM

two.p3=predict(bmi3)[,1]
two.o3=bmi3$data$OUT_MI
four.p3=predict(bmi4)[,1]
four.o3=bmi4$data$OUT_MI
cont.p3=predict(bmi2)[,1]
cont.o3=bmi2$data$OUT_MI

two.p4=predict(bcabg3)[,1]
two.o4=bcabg3$data$OUT_CABG
four.p4=predict(bcabg4)[,1]
four.o4=bcabg4$data$OUT_CABG
cont.p4=predict(bcabg2)[,1]
cont.o4=bcabg2$data$OUT_CABG

two.p5=predict(bpci3)[,1]
two.o5=bpci3$data$OUT_PCI
four.p5=predict(bpci4)[,1]
four.o5=bpci4$data$OUT_PCI
cont.p5=predict(bpci2)[,1]
cont.o5=bpci2$data$OUT_PCI

two.p6=predict(bmace3)[,1]
two.o6=bmace3$data$OUT_MACE
four.p6=predict(bmace4)[,1]
four.o6=bmace4$data$OUT_MACE
cont.p6=predict(bmace2)[,1]
cont.o6=bmace2$data$OUT_MACE

two.p7=predict(bae3)[,1]
two.o7=bae3$data$OUT_AE
four.p7=predict(bae4)[,1]
four.o7=bae4$data$OUT_AE
cont.p7=predict(bae2)[,1]
cont.o7=bae2$data$OUT_AE

predval<-round(matrix(c(
mean(ifelse(two.p==two.o,1,0)),
mean(ifelse(four.p==two.o,1,0)),
mean(ifelse(cont.p==cont.o,1,0)),
mean(ifelse(two.p2==two.o2,1,0)),
mean(ifelse(four.p2==two.o2,1,0)),
mean(ifelse(cont.p2==cont.o2,1,0)),
mean(ifelse(two.p3==two.o3,1,0)),
mean(ifelse(four.p3==two.o3,1,0)),
mean(ifelse(cont.p3==cont.o3,1,0)),
mean(ifelse(two.p4==two.o4,1,0)),
mean(ifelse(four.p4==two.o4,1,0)),
mean(ifelse(cont.p4==cont.o4,1,0)),
mean(ifelse(two.p5==two.o5,1,0)),
mean(ifelse(four.p5==two.o5,1,0)),
mean(ifelse(cont.p5==cont.o5,1,0)),
mean(ifelse(two.p6==two.o6,1,0)),
mean(ifelse(four.p6==two.o6,1,0)),
mean(ifelse(cont.p6==cont.o6,1,0)),
mean(ifelse(two.p7==two.o7,1,0)),
mean(ifelse(four.p7==two.o7,1,0)),
mean(ifelse(cont.p7==cont.o7,1,0))),3,7,byrow=F),2)
write.csv(predval,file="predval.csv")

briers<-round(matrix(c(
  mean((two.p1-two.o1)^2),
  mean((four.p1-four.o1)^2),
  mean((cont.p1-cont.o1)^2),
  mean((two.p2-two.o2)^2),
  mean((four.p2-four.o2)^2),
  mean((cont.p2-cont.o2)^2),
  mean((two.p3-two.o3)^2),
  mean((four.p3-four.o3)^2),
  mean((cont.p3-cont.o3)^2),
  mean((two.p4-two.o4)^2),
  mean((four.p4-four.o4)^2),
  mean((cont.p4-cont.o4)^2),
  mean((two.p5-two.o5)^2),
  mean((four.p5-four.o5)^2),
  mean((cont.p5-cont.o5)^2),
  mean((two.p6-two.o6)^2),
  mean((four.p6-four.o6)^2),
  mean((cont.p6-cont.o6)^2),
  mean((two.p7-two.o7)^2),
  mean((four.p7-four.o7)^2),
  mean((cont.p7-cont.o7)^2)),3,7,byrow=F),3)
write.csv(briers,file="briers.csv")


# Assumptions bayesian models: rhat & ess
cbind(
  round(rhat(bacm2),3)[1:9],
  round(rhat(bcm2),3)[1:9],
  round(rhat(bmi2),3)[1:9],
  round(rhat(bcabg2),3)[1:9],
  round(rhat(bpci2),3)[1:9],
  round(rhat(bmace2),3)[1:9],
  round(rhat(bae2),3)[1:9])


# Gather results for moderation of disease
disdat<-data.frame(
  acm=paste0(round(exp(summary(bacm9)$fixed[11:15,1]),3)," [",round(exp(summary(bacm9)$fixed[11:15,3]),3),", ",round(exp(summary(bacm9)$fixed[11:15,4]),3),"]"),
  cm=paste0(round(exp(summary(bcm9)$fixed[11:15,1]),3)," [",round(exp(summary(bcm9)$fixed[11:15,3]),3),", ",round(exp(summary(bcm9)$fixed[11:15,4]),3),"]"),
  mi=paste0(round(exp(summary(bmi9)$fixed[11:15,1]),3)," [",round(exp(summary(bmi9)$fixed[11:15,3]),3),", ",round(exp(summary(bmi9)$fixed[11:15,4]),3),"]"),
  cabg=paste0(round(exp(summary(bcabg9)$fixed[11:15,1]),3)," [",round(exp(summary(bcabg9)$fixed[11:15,3]),3),", ",round(exp(summary(bcabg9)$fixed[11:15,4]),3),"]"),
  pci=paste0(round(exp(summary(bpci9)$fixed[11:15,1]),3)," [",round(exp(summary(bpci9)$fixed[11:15,3]),3),", ",round(exp(summary(bpci9)$fixed[11:15,4]),3),"]"),
  mace=paste0(round(exp(summary(bmace9)$fixed[11:15,1]),3)," [",round(exp(summary(bmace9)$fixed[11:15,3]),3),", ",round(exp(summary(bmace9)$fixed[11:15,4]),3),"]"),
  ae=paste0(round(exp(summary(bae9)$fixed[11:15,1]),3)," [",round(exp(summary(bae9)$fixed[11:15,3]),3),", ",round(exp(summary(bae9)$fixed[11:15,4]),3),"]")
)
write.csv(disdat,file="disdat.csv")


################################
######## OTHER PRIORS ##########

##### Large variance normal priors #######
priors2w<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors2w[2:9,1]<-"normal(0,4)"
priors2w<-priors2[-(dim(priors2w)[1]),]

fbacm3 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbcm3 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbmi3 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbcagb3 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbpci3 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbmace3 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbae3 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2w, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_wide<-list(fbacm3,fbcm3,fbmi3,fbmace3,fbae3,fbcagb3,fbpci3)
save(res_wide,file="res_wide.rdata")

##### Small variance normal priors #######
priors2n<-get_prior(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat)
priors2n[2:9,1]<-"normal(0,1)"
priors2n<-priors2[-(dim(priors2w)[1]),]
fbacm4 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbcm4 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbmi4 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbcagb4 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbpci4 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbmace4 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
fbae4 <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat,sample_prior=T,prior=priors2n, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_wide<-list(fbacm4,fbcm4,fbmi4,fbmace4,fbae4,fbcagb4,fbpci4)
save(res_wide,file="res_wide.rdata")

##############################
#### HF and CAD subgroups ####

bacm.cad <- brm(OUT_ACM ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2, sample_prior=T, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bacm.hf <- brm(OUT_ACM ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bcm.cad <- brm(OUT_CM ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2,sample_prior=T, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bcm.hf <- brm(OUT_CM ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bmi.cad <- brm(OUT_MI ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2, sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bmi.hf <- brm(OUT_MI ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bcabg.cad <- brm(OUT_CABG ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2, sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bcabg.hf <- brm(OUT_CABG ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bpci.cad <- brm(OUT_PCI ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=20)) 
bpci.hf <- brm(OUT_PCI ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2,sample_prior=T, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=20)) 
bmace.cad <- brm(OUT_MACE ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2,sample_prior=T, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bmace.hf <- brm(OUT_MACE ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bae.cad <- brm(OUT_AE ~ FOLLOWUP_months +AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="CAD"),], family = bernoulli("logit"),prior=priors2,sample_prior=T, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bae.hf <- brm(OUT_AE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat[which(dat$DISEASE=="HF"),], family = bernoulli("logit"), prior=priors2 ,sample_prior=T,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
cadhf<-list(bacm.cad,bacm.hf,bcm.cad,bcm.hf,bmi.cad,bpci.cad,bmace.cad,bmace.hf,bae.cad,bae.hf)
save(cadhf,file="cadhf.rdata")

bcabg7 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci7 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bnasis<-list(bae7,bmace7,bacm7,bcm7,bmi7,bpci7,bcabg7)
save(bnasis,file="resnasi.rdata")

# Bayes factors
hypothesis(bacm.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bacm.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bacm.hf, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bacm.hf, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcm.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcm.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcm.hf, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcm.hf, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmi.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmi.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcabg.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bcabg.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bpci.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bpci.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmace.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmace.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmace.hf, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bmace.hf, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bae.cad, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bae.cad, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bae.hf, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio
hypothesis(bae.hf, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio


#### Function to compute posterior probability of Type D effect > 0 ####
bfmat<-matrix(c(

# ACM
hypothesis(bacm2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bacm2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bacm3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bacm4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bacm4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bacm4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,


# CM
hypothesis(bcm2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bcm2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bcm3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcm4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcm4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcm4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,

# MI
hypothesis(bmi2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bmi2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bmi3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmi4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmi4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmi4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,

# CABG
hypothesis(bcabg2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bcabg2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bcabg3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcabg4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcabg4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bcabg4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,

# PCI
hypothesis(bpci2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bpci2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bpci3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bpci4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bpci4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bpci4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,

# MACE
hypothesis(bmace2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bmace2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bmace3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmace4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmace4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bmace4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio,

# AE
hypothesis(bae2, "b_NASI = 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bae2, "b_NASI > 0", class = NULL,sample_prior="yes")$hypothesis$Evid.Ratio,
hypothesis(bae3, "b_TYPED2 > 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bae4, "b_HighNA < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bae4, "b_HighSI < 0", class = NULL)$hypothesis$Evid.Ratio,
hypothesis(bae4, "b_Ref < 0", class = NULL)$hypothesis$Evid.Ratio)

,6,7,byrow=F)
round(bfmat,2)



#### Results table multilevel model estimates ####

rs<-round(exp(rbind(
  cbind(summary(bacm0)$fixed[1:6,c(1,3,4)],summary(bcm0)$fixed[1:6,c(1,3,4)],summary(bmi0)$fixed[1:6,c(1,3,4)],summary(bcabg0)$fixed[1:6,c(1,3,4)],summary(bpci0)$fixed[1:6,c(1,3,4)],summary(bmace0)$fixed[1:6,c(1,3,4)],summary(bae0)$fixed[1:6,c(1,3,4)]),
  cbind(summary(bacm1)$fixed[7:8,c(1,3,4)],summary(bcm1)$fixed[7:8,c(1,3,4)],summary(bmi1)$fixed[7:8,c(1,3,4)],summary(bcabg1)$fixed[7:8,c(1,3,4)],summary(bpci1)$fixed[7:8,c(1,3,4)],summary(bmace1)$fixed[7:8,c(1,3,4)],summary(bae1)$fixed[7:8,c(1,3,4)]),
  c(summary(bacm2)$fixed[9,c(1,3,4)],summary(bcm2)$fixed[9,c(1,3,4)],summary(bmi2)$fixed[9,c(1,3,4)],summary(bcabg2)$fixed[9,c(1,3,4)],summary(bpci2)$fixed[9,c(1,3,4)],summary(bmace2)$fixed[9,c(1,3,4)],summary(bae2)$fixed[9,c(1,3,4)]),
  c(summary(bacm3)$fixed[5,c(1,3,4)],summary(bcm3)$fixed[5,c(1,3,4)],summary(bmi3)$fixed[5,c(1,3,4)],summary(bcabg3)$fixed[5,c(1,3,4)],summary(bpci3)$fixed[5,c(1,3,4)],summary(bmace3)$fixed[5,c(1,3,4)],summary(bae3)$fixed[5,c(1,3,4)]),
  cbind(summary(bacm4)$fixed[5:7,c(1,3,4)],summary(bcm4)$fixed[5:7,c(1,3,4)],summary(bmi4)$fixed[5:7,c(1,3,4)],summary(bcabg4)$fixed[5:7,c(1,3,4)],summary(bpci4)$fixed[5:7,c(1,3,4)],summary(bmace4)$fixed[5:7,c(1,3,4)],summary(bae4)$fixed[5:7,c(1,3,4)]),
  cbind(summary(bacm5)$fixed[10:14,c(1,3,4)],summary(bcm5)$fixed[10:14,c(1,3,4)],summary(bmi5)$fixed[10:14,c(1,3,4)],summary(bcabg5)$fixed[10:14,c(1,3,4)],summary(bpci5)$fixed[10:14,c(1,3,4)],summary(bmace5)$fixed[10:14,c(1,3,4)],summary(bae5)$fixed[10:14,c(1,3,4)]),
  cbind(summary(bacm6)$fixed[10:14,c(1,3,4)],summary(bcm6)$fixed[10:14,c(1,3,4)],summary(bmi6)$fixed[10:14,c(1,3,4)],summary(bcabg6)$fixed[10:14,c(1,3,4)],summary(bpci6)$fixed[10:14,c(1,3,4)],summary(bmace6)$fixed[10:14,c(1,3,4)],summary(bae6)$fixed[10:14,c(1,3,4)])
  
  )),3)
write.csv(rs,file="Dres.csv")

# Results table random parameters
randoms<-data.frame(acm=paste0(round(summary(bacm2)$random$DID[,1],2)," [",round(summary(bacm2)$random$DID[,3],2),", ",round(summary(bacm2)$random$DID[,4],2),"]"),
                    cm=paste0(round(summary(bcm2)$random$DID[,1],2)," [",round(summary(bcm2)$random$DID[,3],2),", ",round(summary(bcm2)$random$DID[,4],2),"]"),
                    mi=paste0(round(summary(bmi2)$random$DID[,1],2)," [",round(summary(bmi2)$random$DID[,3],2),", ",round(summary(bmi2)$random$DID[,4],2),"]"),
                    cabg=paste0(round(summary(bcabg2)$random$DID[,1],2)," [",round(summary(bcabg2)$random$DID[,3],2),", ",round(summary(bcabg2)$random$DID[,4],2),"]"),
                    pci=paste0(round(summary(bpci2)$random$DID[,1],2)," [",round(summary(bpci2)$random$DID[,3],2),", ",round(summary(bpci2)$random$DID[,4],2),"]"),
                    mace=paste0(round(summary(bmace2)$random$DID[,1],2)," [",round(summary(bmace2)$random$DID[,3],2),", ",round(summary(bmace2)$random$DID[,4],2),"]"),
                    ae=paste0(round(summary(bae2)$random$DID[,1],2)," [",round(summary(bae2)$random$DID[,3],2),", ",round(summary(bae2)$random$DID[,4],2),"]"))
write.csv(randoms,"randompars.csv")


#### Visualize interaction MACE ####

## Re-run model excluding non significant quadratic terms
bmace <- brm(OUT_MACE ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bmi <- brm(OUT_MI ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bacm <- brm(OUT_ACM ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bcm <- brm(OUT_CM ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 

bmace %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) -> preds


# Plot results allcause mortality
bacm %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = NA_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("Predicted probability") +
  xlab("")+
  ggtitle("All-cause mortality")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Social inhibition (SI)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->pacm

# Plot results cardiac mortality
bcm2 %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = NA_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("") +
  xlab("")+
  ggtitle("Cardiac mortality")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Social inhibition (SI)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->pcm

# Plot results myocardial infarction
bmi7 %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = NA_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("Predicted probability") +
  xlab("Z-score negative affectivity (NA)")+
  ggtitle("Myocardial infarction")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Social inhibition (SI)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->pmi

# Plot results MACE
bmace2 %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = NA_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("") +
  xlab("Z-score negative affectivity (NA)")+
  ggtitle("Major adverse cardiac event")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Social inhibition (SI)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->pmace


# Plot results AE (across NA)
bae %>%
  spread_draws(b_Intercept, b_NAst,b_SIst,b_NASI) %>%
  mutate(NA_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(NA_st) %>%
  #mutate(SI_st = list(c(-1,0,1))) %>% 
  #unnest(SI_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(-1)+ b_NASI*NA_st*(-1))),
         pred2 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(0)+ b_NASI*NA_st*(0))),
         pred3 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(1)+ b_NASI*NA_st*(1))),
         pred4 = exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2))/(1+exp(b_Intercept + b_NAst*NA_st + b_SIst*(2)+ b_NASI*NA_st*(2)))) %>%
  group_by(NA_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = NA_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("") +
  xlab("Z-score negative affectivity (NA)")+
  ggtitle("Any adverse event")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Social inhibition (SI)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->paena


# Plot results AE (across SI)
bae %>%
  spread_draws(b_Intercept, b_SIst,b_NAst,b_NASI) %>%
  mutate(SI_st = list(seq(-4, 4, 0.01))) %>% #the observed value range of NA
  unnest(SI_st) %>%
  #mutate(NA_st = list(c(-1,0,1))) %>% 
  #unnest(NA_st) %>% 
  mutate(pred1 = exp(b_Intercept + b_SIst*SI_st + b_NAst*(-1)+ b_NASI*SI_st*(-1))/(1+exp(b_Intercept + b_SIst*SI_st + b_NAst*(-1)+ b_NASI*SI_st*(-1))),
         pred2 = exp(b_Intercept + b_SIst*SI_st + b_NAst*(0)+ b_NASI*SI_st*(0))/(1+exp(b_Intercept + b_SIst*SI_st + b_NAst*(0)+ b_NASI*SI_st*(0))),
         pred3 = exp(b_Intercept + b_SIst*SI_st + b_NAst*(1)+ b_NASI*SI_st*(1))/(1+exp(b_Intercept + b_SIst*SI_st + b_NAst*(1)+ b_NASI*SI_st*(1))),
         pred4 = exp(b_Intercept + b_SIst*SI_st + b_NAst*(2)+ b_NASI*SI_st*(2))/(1+exp(b_Intercept + b_SIst*SI_st + b_NAst*(2)+ b_NASI*SI_st*(2)))) %>%
  group_by(SI_st) %>%
  summarise(pred1_m = mean(pred1, na.rm = TRUE),
            pred1_low = quantile(pred1, prob = 0.025),
            pred1_high = quantile(pred1, prob = 0.975),
            pred2_m = mean(pred2, na.rm = TRUE),
            pred2_low = quantile(pred2, prob = 0.025),
            pred2_high = quantile(pred2, prob = 0.975),
            pred3_m = mean(pred3, na.rm = TRUE),
            pred3_low = quantile(pred3, prob = 0.025),
            pred3_high = quantile(pred3, prob = 0.975),
            pred4_m = mean(pred4, na.rm = TRUE),
            pred4_low = quantile(pred4, prob = 0.025),
            pred4_high = quantile(pred4, prob = 0.975)) %>%
  ggplot(aes(x = SI_st, y = pred1_m)) +
  geom_line(aes(linetype="Z = -1"),col="yellow",lwd=1.25) +
  geom_line(aes(y=pred2_m,linetype="Z = 0"),col="orange",lwd=1.25)+
  geom_line(aes(y=pred3_m,linetype="Z = 1"),col="red",lwd=1.25)+
  geom_line(aes(y=pred4_m,linetype="Z = 2"),col="darkred",lwd=1.25)+
  geom_ribbon(aes(ymin = pred1_low, ymax = pred1_high), alpha=0.15,fill="yellow") +
  geom_ribbon(aes(ymin = pred2_low, ymax = pred2_high), alpha=0.15,fill="orange") +
  geom_ribbon(aes(ymin = pred3_low, ymax = pred3_high), alpha=0.15,fill="red") +
  geom_ribbon(aes(ymin = pred4_low, ymax = pred4_high), alpha=0.15,fill="darkred") +
  ylab("") +
  xlab("Z-score social inhibition (SI)”)+
  ggtitle(“Any adverse event")+
  scale_y_continuous(breaks = seq(0, 1, 0.1),lim=c(0,1))+
  # ylim(0,1)+
  theme_minimal(base_size=15)+
  scale_linetype_manual(name = "Negative affectivity (NA)",values=c(1,2,3,4),label = c("Z = -1","Z = 0","Z = 1","Z = 2"), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("yellow", "orange", "red","darkred"),
                                                    size = c(1.15,1.15,1.15,1.15))))->paesi




pdf(file="intplotcm.pdf",height=4,width=7)
intplot
dev.off()

pdf(file="intplotscad.pdf",height=8,width=10)
ggarrange(pacm,pcm,pmi,pmace,ncol=2,nrow=2,common.legend=TRUE,legend="right")
dev.off()

#pmi<-ggplot(dat)+geom_blank()
pdf(file="intplotshf.pdf",height=5,width=12)
ggarrange(pacm,pcm,pmace,ncol=3,nrow=1,common.legend=TRUE,legend="bottom")
dev.off()



###################################
#### Two-level meta-analysis   ####
###################################

endp<-c("OUT_ACM","OUT_CM","OUT_MI","OUT_CABG","OUT_PCI","OUT_MACE","OUT_AE")
ors<-matrix(NA,length(unique(dat$DID)),length(endp))
colnames(ors)<-endp
rownames(ors)<-didtable[,2]

# Loop bayesian analyses over all outcomes and studies
for(s in 1:length(unique(dat$DID))){
    temp<-subset(dat,dat$DID==s)
      for(e in 1:length(endp)){
        tempout<-endp[e]
        if(sum(is.na(temp[,tempout]))==length(temp[,tempout])){ors[s,e]<-NA
        }else{
          tempform<-as.formula(as.character(paste0(tempout," ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI")))
          tempres <- brm(tempform, data = temp,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
          ors[s,e]<-paste0(round(exp(summary(tempres)$fix["NASI",c(1)]),3),", (",round(exp(summary(tempres)$fix["NASI",c(3)]),3),", ",round(exp(summary(tempres)$fix["NASI",c(4)]),3),")")
          rm(tempres)
      }
  }
}

# Loop regular logistic regression analyses over all outcomes and studies
orsf<-matrix(NA,length(unique(dat$DID)),length(endp))
orse<-matrix(NA,length(unique(dat$DID)),length(endp))
orss<-matrix(NA,length(unique(dat$DID)),length(endp))
colnames(orsf)<-endp
rownames(orsf)<-didtable[,2]
for(s in 1:length(unique(dat$DID))){
  temp<-subset(dat,dat$DID==s)
  for(e in 1:length(endp)){
    tempout<-endp[e]
    if(sum(is.na(temp[,tempout]))==length(temp[,tempout])){ors[s,e]<-NA
    }else{
      if(length(table(temp[,tempout]))<2){ors[s,e]<-NA
      }else{
      tempform<-as.formula(as.character(paste0(tempout," ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI")))
      tempres <- glm(tempform, data = temp,family = binomial("logit")) 
      orsf[s,e]<-paste0(round(exp(summary(tempres)$coefficients["NASI",c(1)]),2),", (",round(exp(    summary(tempres)$coefficients["NASI",c(1)]-1.96*summary(tempres)$coefficients["NASI",c(2)]),2),", ",round(exp(    summary(tempres)$coefficients["NASI",c(1)]+1.96*summary(tempres)$coefficients["NASI",c(2)]),2),")")
      orse[s,e]<-round(summary(tempres)$coefficients["NASI",c(1)],2)
      orss[s,e]<-round(summary(tempres)$coefficients["NASI",c(2)],2)
      rm(tempres)
      }
    }
  }
}
write.csv(orsf,file="studyors.csv")

# Meta-analysis
orse[which(orss>100)]<-NA
orss[which(orss>3)]<-NA
ral<-mal<-fors<-list()
for(e in 1:length(endp)){
  print(e)
  mal[[e]]<-rma.uni(yi=orse[,e],sei=orss[,e],method="FE")
  ral[[e]]<-rma.uni(yi=orse[,e],sei=orss[,e])
  fors[[e]]<-forest(mal[[e]],transf=exp,refline=1,main=endp[e],slab=didtable[,4],xlab="Odds ratio")

}

pdf(file=paste0("forest_acm.pdf"),width=8)
forest(mal[[1]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="All-cause mortality",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[1]]$k - mal[[1]]$p),,") = ",
                                            .(formatC(mal[[1]]$QE, digits=2, format="f")),  
                                            ", p = ", .(formatC(mal[[1]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(mal[[1]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_cm.pdf"),width=8)
forest(mal[[2]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Cardiac mortality",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[2]]$k - mal[[2]]$p),,") = ",
                                             .(formatC(mal[[2]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[2]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[2]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_mi.pdf"),width=8)
forest(mal[[3]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Myocardial infarction",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[3]]$k - mal[[3]]$p),,") = ",
                                             .(formatC(mal[[3]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[3]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[3]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_cabg.pdf"),width=8)
forest(mal[[4]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),clim=c(0,10),at=0:6,main="Coronary artery bypass grafting",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[4]]$k - mal[[4]]$p),,") = ",
                                             .(formatC(mal[[4]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[4]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[4]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_pci.pdf"),width=8)
forest(mal[[5]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Percutaneous coronary intervention",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[5]]$k - mal[[5]]$p),,") = ",
                                             .(formatC(mal[[5]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[5]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[5]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_mace.pdf"),width=8)
forest(mal[[6]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Major adverse cardiac events",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[6]]$k - mal[[6]]$p),,") = ",
                                             .(formatC(mal[[6]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[6]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[6]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_ae.pdf"),width=8)
forest(mal[[7]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Any adverse events",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[7]]$k - mal[[7]]$p),,") = ",
                                             .(formatC(mal[[7]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[7]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[7]]$I2, digits=1, format="f")), "%")))
dev.off()



#############################
# Leave 1 out Meta-analysis #
#############################

oneout<-oneouts<-oneoutp<-oneoutl<-oneoutu<-oneoutf<-matrix(NA,nrow=dim(didtable)[1],ncol=length(endp))
for(e in 1:length(endp)){
  for(s in 1:dim(didtable)[1]){
    print(paste0("Endpoint ",e,", Study ",s))
    temp<-rma.uni(yi=orse[-s,e],sei=orss[-s,e],method="FE")
    oneout[s,e]<-temp$beta
    oneoutp[s,e]<-temp$pval
    oneoutl[s,e]<-temp$ci.lb
    oneoutu[s,e]<-temp$ci.ub
    oneoutf[s,e]<-paste0(round(exp(temp$beta),2)," [",round(exp(temp$ci.lb),2),", ",round(exp(temp$ci.ub),2),"]")
    
  }
}

# clean up results
oneouts<-oneoutp<.05
oneout[which(is.na(orss))]<-NA
oneoutp[which(is.na(orss))]<-NA
oneouts[which(is.na(orss))]<-NA
oneoutl[which(is.na(orss))]<-NA
oneoutu[which(is.na(orss))]<-NA
oneoutf[which(is.na(orss))]<-NA

# Print to csv file
write.csv(cbind(as.character(didtable[,4]),oneoutf),file="leave1out.csv")


###############################
#### Simple slope analysis ####
###############################

s1<-glmer(OUT_AE ~ AGE + SEX + NAst*SIst + 
            (1|DID) , data = dat, family = binomial("logit"))
ss1<-simple_slopes(s1,confint=T,levels=list(NAst=c(-1,-0.75,-0.5,-0.25,0, 0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3, 'sstest'), SIst=c(-1, 0, 1, 'sstest')))
s2<-glmer(OUT_AE ~ AGE + SEX + SIst*NAst + 
            (1|DID) , data = dat, family = binomial("logit"))
ss2<-simple_slopes(s2,confint=T,levels=list(SIst=c(-1, 'sstest'), NAst=c(-1, 0, 1, 'sstest')))

slopess<-cbind(paste0(round(as.numeric(ss1$SIst),2)," [",round(as.numeric(ss1[,5]),2),", ",round(as.numeric(ss1[,6]),2),"]"),
               paste0(round(as.numeric(ss2$NAst),2)," [",round(as.numeric(ss2[,5]),2),", ",round(as.numeric(ss2[,6]),2),"]"))
write.csv(slopess,file="simpleslopes.csv")

