#############################################################################
# Individual patient data meta-analysis Type D personality & Adverse events #
#                                                                           #
# Script written by: Paul Lodder                                             #
# Version: October 16th 2020                                                #
#                                                                           #
##############################################################################

#### Set working directory and load libraries ####
setwd("~/Google Drive/Work/Research/PhD projects/Project7_IPDmeta-analysis/Datasets")
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
dat<-read.csv(file="IPDMA_17.csv",header=T,sep=";",dec=",")
dat$AGE<-scale(as.numeric(as.character(dat$AGE)))
dat$Dataset<-factor(dat$Dataset)
#dat<-read.xlsx(file="IPDMA_5.xlsx",sheetIndex = 1,header=T)
head(dat)
names(dat)
dim(dat)

#### Create numeric study ID and label variable ####
didtable<-data.frame(code=levels(dat$Dataset),
                     year=c(2011,2016,2011,2014,2015,2015,2016,2017,2017,2019,2011,1996,2013,2013,2000,2010,2010,2010),
                     label=factor(c("Coyne et al. (2011)","Albus et al. (2016)","Grande et al. (2011)","Meyer et al. (2014)","Sumin et al. (2015)",
                             "Dulfer et al. (2015)","Gostoli et al. (2016)","Pushkarev et al. (2017)","Conden et al. (2017)","Lin et al. (2019)",
                             "Schmidt et al. (2011)","Denollet et al. (1996)","Denollet et al. (2013a)","Denollet et al. (2013b)","Denollet et al. (2000)",
                             "Pedersen et al. (2010)","Martens et al. (2010)","Pelle et al. (2010)")
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
describe(dat)



##################################
# Make figures with descriptives #
##################################

#### Plot NA and SI score distributions ####

### Overall ###

# Correlation NA & SI
cor(dat$NAst,dat$SIst, use="complete.obs")

# Overall scatterplot NA & SI, with 4 Type D groups as color
scatdat<-na.omit(dat[,c("NASO","SISO","TYPED4")])
ggplot(scatdat,aes(x=NASO,y=SISO,color=TYPED4))+
  geom_count()+
  theme_minimal(base_size=17)+
  xlab("Negative affectivity")+ylab("Social inhibition")

# Histograms NA & SI
layout(rbind(1:2))
par(mar=c(4,4,2,2))
hist(dat$NAst,breaks=50,main="NA",xlab="",las=1)
hist(dat$SIst,breaks=50,main="SI",xlab="",las=1)



### Per dataset ###

# NA
dev.off()
layout(matrix(1:20,4,5,byrow=T))
par(mar=c(2,1,2,1))
plot.new()
mtext("NA",font=2,cex=1.5)
for(i in 1:18){
  temp<-subset(dat,dat$DID==i)
  hist(temp$NAst,xlab="",ylab="",breaks=50,main=temp$Dataset[1])
}

# SI
dev.off()
layout(matrix(1:20,4,5,byrow=T))
par(mar=c(2,1,2,1))
plot.new()
mtext("SI",font=2,cex=1.5)
for(i in 1:18){
  temp<-subset(dat,dat$DID==i)
  hist(temp$SIst,xlab="",ylab="",breaks=50,main=temp$Dlabel[1])
}

# NA & SI Densities
pdf(file="ipdma_densities.pdf",width=8,height=10)
layout(matrix(1:20,5,4,byrow=T))
par(mar=c(2,2,2,1))
for(i in 1:18){
  print(i)
  temp<-subset(dat,dat$DID==i)
  plot(density(temp$NAst,na.rm=T),cex.lab=.8,col="blue",lwd=1.8,lty=2,ylim=c(0,1.2),xlim=c(-3,3),main=temp$Dlabel[1])
  lines(density(temp$SIst,na.rm=T),col="orange",lwd=1.8,lty=1,xlim=c(-3,3))
}
plot.new()
legend(0,1,c("NA","SI"),col=c("blue","orange"),lty=c(2,1),lwd=2,cex=1.6)
plot.new()
dev.off()

# NA & SI Histograms
pdf(file="ipdma_histsNA.pdf",width=8,height=10)
layout(matrix(1:18,6,3,byrow=T))
par(mar=c(2,2,2,1))
for(i in 1:18){
  print(i)
  temp<-subset(dat,dat$DID==i)
  hist(temp$NASO,na.rm=T,cex.lab=.8,breaks=25,col=rgb(0,0,1,.4),xlim=c(0,28),main=temp$Dlabel[1])
}

dev.off()

pdf(file="ipdma_histsSI.pdf",width=8,height=10)
layout(matrix(1:18,6,3,byrow=T))
par(mar=c(2,2,2,1))
for(i in 1:18){
  print(i)
  temp<-subset(dat,dat$DID==i)
  hist(temp$SISO,na.rm=T,cex.lab=.8,breaks=25,col=rgb(1,0.65,0,.4),xlim=c(0,28),main=temp$Dlabel[1])
}

dev.off()


# Correlation between NA & SI scores 
dat %>% 
  group_by(Dataset) %>% 
  dplyr::summarize(cor(NAst,SIst, use="complete.obs"))



# Scatterplot NA & SI
plotlist=list()
for(i in 1:18){
  temp<-subset(dat,dat$DID==i)
  if(i %in% c(1,4,7,10,13)){
    plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
      geom_count()+
      theme_minimal(base_size=13)+
      xlim(0,28)+
      ylim(0,28)+
      theme(legend.position="none")+
      xlab("")+ylab("SI")+ggtitle(temp$Dlabel[1])
  } else {
    if(i %in% c(17,18)){
      plotlist[[i]]<-ggplot(temp,aes(x=NASO,y=SISO))+
        geom_count()+
        xlim(0,28)+
        ylim(0,28)+
        theme_minimal(base_size=13)+
        theme(legend.position="none")+
        xlab("NA")+ylab("")+ggtitle(temp$Dlabel[1])
    } else {
      if(i %in% c(16)){
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
             plotlist[[16]],plotlist[[17]],plotlist[[18]],ncol=3,nrow=6)
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

#### ACM ####
bacm0 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm1 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm2 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm3 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm4 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm5 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm6 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bacm7 <- brm(OUT_ACM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_acm<-list(bacm0,bacm1,bacm2,bacm3,bacm4,bacm5,bacm6)
save(res_acm,file="res_acm.rdata")
   
#### CM ####
bcm0 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm1 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm2 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm3 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm4 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm5 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bcm6 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)res_cm<-list(bcm0,bcm1,bcm2,bcm3,bcm4,bcm5,bcm6)
bcm7 <- brm(OUT_CM ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_cm<-list(bcm0,bcm1,bcm2,bcm3,bcm4,bcm5,bcm6)
save(res_cm,file="res_cm.rdata")

#### MI ####
bmi0 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi1 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi2 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi3 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi4 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi5 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmi6 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)res_cm<-list(bcm0,bcm1,bcm2,bcm3,bcm4,bcm5,bcm6)
bmi7 <- brm(OUT_MI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_mi<-list(bmi0,bmi1,bmi2,bmi3,bmi4,bmi5,bmi6)
save(res_mi,file="res_mi.rdata")

#### MACE ####
bmace0 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + (1 + AGE + SEX + NAst + SIst || DID), data = dat, family = bernoulli("logit"), prior=priors0, warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace1 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 || DID), data = dat, prior=priors1, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace2 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, prior=priors2,sample_prior=T,family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace3 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + TYPED2 + (1 + AGE + SEX + TYPED2 || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors3,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace4 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + Ref + HighSI + HighNA + (1 + AGE + SEX + Ref + HighSI + HighNA || DID), data = dat, family = bernoulli("logit"), prior=priors4,warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace5 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*SEX + SIst*SEX + NA2*SEX + SI2*SEX + NASI*SEX|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors5,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bmace6 <- brm(OUT_MACE ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + NAst*AGE + SIst*AGE + NA2*AGE + SI2*AGE + NASI*AGE|| DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors6,  iter   = 2000, chains = 3, inits  = "random", cores  = 4)res_cm<-list(bcm0,bcm1,bcm2,bcm3,bcm4,bcm5,bcm6)
bmace7 <- brm(OUT_MACE ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
res_mace<-list(bmace0,bmace1,bmace2,bmace3,bmace4,bmace5,bmace6)
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
res_acm<-list(bae0,bae1,bae2,bae3,bae4,bae5,bae6)
save(res_ae,file="res_ae.rdata")

#### CABG PCI ####
bcabg <- brm(OUT_CABG ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bpci <- brm(OUT_PCI ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1 + AGE + SEX + NAst + SIst + NA2 + SI2 + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=20)) 
bcabg7 <- brm(OUT_CABG ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bpci7 <- brm(OUT_PCI ~ FOLLOWUP_months + AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,prior=priors7,  iter   = 2000, chains = 3, inits  = "random", cores  = 4) 
bnasis<-list(bae7,bmace7,bacm7,bcm7,bmi7,bpci7,bcabg7)
save(bnasis,file="resnasi.rdata")

#### Function to compute posterior probability of Type D effect > 0 ####
setwd("~/surfdrive/Study/PhD/Project7_IPDMA/IPDMA Results")
load("res_acm.rdata")
load("res_ae.rdata")
load("res_cm.rdata")
load("res_mi.rdata")
load("res_mace.rdata")
bacm0<-res_acm[[1]]
bacm1<-res_acm[[2]]
bacm2<-res_acm[[3]]
bacm3<-res_acm[[4]]
bacm4<-res_acm[[5]]
bacm5<-res_acm[[6]]
bacm6<-res_acm[[7]]
bae0<-res_ae[[1]]
bae1<-res_ae[[2]]
bae2<-res_ae[[3]]
bae3<-res_ae[[4]]
bae4<-res_ae[[5]]
bae5<-res_ae[[6]]
bae6<-res_ae[[7]]
bcm0<-res_cm[[1]]
bcm1<-res_cm[[2]]
bcm2<-res_cm[[3]]
bcm3<-res_cm[[4]]
bcm4<-res_cm[[5]]
bcm5<-res_cm[[6]]
bcm6<-res_cm[[7]]
bmi0<-res_mi[[1]]
bmi1<-res_mi[[2]]
bmi2<-res_mi[[3]]
bmi3<-res_mi[[4]]
bmi4<-res_mi[[5]]
bmi5<-res_mi[[6]]
bmi6<-res_mi[[7]]
bmace0<-res_mace[[1]]
bmace1<-res_mace[[2]]
bmace2<-res_mace[[3]]
bmace3<-res_mace[[4]]
bmace4<-res_mace[[5]]
bmace5<-res_mace[[6]]
bmace6<-res_mace[[7]]

# ACM
hypothesis(bacm2, "b_NASI = 0", class = NULL,sample_prior="yes")
hypothesis(bacm2, "b_NASI > 0", class = NULL,sample_prior="yes")
hypothesis(bacm3, "b_TYPED2 > 0", class = NULL)
hypothesis(bacm4, "b_HighNA < 0", class = NULL)
hypothesis(bacm4, "b_HighSI < 0", class = NULL)
hypothesis(bacm4, "b_Ref < 0", class = NULL)

# CM
hypothesis(bcm2, "b_NASI = 0", class = NULL,sample_prior="yes")
hypothesis(bcm2, "b_NASI > 0", class = NULL,sample_prior="yes")
hypothesis(bcm3, "b_TYPED2 > 0", class = NULL)
hypothesis(bcm4, "b_HighNA < 0", class = NULL)
hypothesis(bcm4, "b_HighSI < 0", class = NULL)
hypothesis(bcm4, "b_Ref < 0", class = NULL)

# MI
hypothesis(bmi2, "b_NASI = 0", class = NULL,sample_prior="yes")
hypothesis(bmi2, "b_NASI > 0", class = NULL,sample_prior="yes")
hypothesis(bmi3, "b_TYPED2 > 0", class = NULL)
hypothesis(bmi4, "b_HighNA < 0", class = NULL)
hypothesis(bmi4, "b_HighSI < 0", class = NULL)
hypothesis(bmi4, "b_Ref < 0", class = NULL)

# MACE
hypothesis(bmace2, "b_NASI = 0", class = NULL,sample_prior="yes")
hypothesis(bmace2, "b_NASI > 0", class = NULL,sample_prior="yes")
hypothesis(bmace3, "b_TYPED2 > 0", class = NULL)
hypothesis(bmace4, "b_HighNA < 0", class = NULL)
hypothesis(bmace4, "b_HighSI < 0", class = NULL)
hypothesis(bmace4, "b_Ref < 0", class = NULL)

# AE
hypothesis(bae2, "b_NASI = 0", class = NULL,sample_prior="yes")
hypothesis(bae2, "b_NASI > 0", class = NULL,sample_prior="yes")
hypothesis(bae3, "b_TYPED2 > 0", class = NULL)
hypothesis(bae4, "b_HighNA < 0", class = NULL)
hypothesis(bae4, "b_HighSI < 0", class = NULL)
hypothesis(bae4, "b_Ref < 0", class = NULL)


#### Results table ####

rs<-round(exp(rbind(
  cbind(summary(bacm0)$fixed[1:6,c(1,3,4)],summary(bcm0)$fixed[1:6,c(1,3,4)],summary(bmi0)$fixed[1:6,c(1,3,4)],summary(bmace0)$fixed[1:6,c(1,3,4)],summary(bae0)$fixed[1:6,c(1,3,4)]),
  cbind(summary(bacm1)$fixed[7:8,c(1,3,4)],summary(bcm1)$fixed[7:8,c(1,3,4)],summary(bmi1)$fixed[7:8,c(1,3,4)],summary(bmace1)$fixed[7:8,c(1,3,4)],summary(bae1)$fixed[7:8,c(1,3,4)]),
  c(summary(bacm2)$fixed[9,c(1,3,4)],summary(bcm2)$fixed[9,c(1,3,4)],summary(bmi2)$fixed[9,c(1,3,4)],summary(bmace2)$fixed[9,c(1,3,4)],summary(bae2)$fixed[9,c(1,3,4)]),
  c(summary(bacm3)$fixed[5,c(1,3,4)],summary(bcm3)$fixed[5,c(1,3,4)],summary(bmi3)$fixed[5,c(1,3,4)],summary(bmace3)$fixed[5,c(1,3,4)],summary(bae3)$fixed[5,c(1,3,4)]),
  cbind(summary(bacm4)$fixed[5:7,c(1,3,4)],summary(bcm4)$fixed[5:7,c(1,3,4)],summary(bmi4)$fixed[5:7,c(1,3,4)],summary(bmace4)$fixed[5:7,c(1,3,4)],summary(bae4)$fixed[5:7,c(1,3,4)]),
  cbind(summary(bacm5)$fixed[10:14,c(1,3,4)],summary(bcm5)$fixed[10:14,c(1,3,4)],summary(bmi5)$fixed[10:14,c(1,3,4)],summary(bmace5)$fixed[10:14,c(1,3,4)],summary(bae5)$fixed[10:14,c(1,3,4)]),
  cbind(summary(bacm6)$fixed[10:14,c(1,3,4)],summary(bcm6)$fixed[10:14,c(1,3,4)],summary(bmi6)$fixed[10:14,c(1,3,4)],summary(bmace6)$fixed[10:14,c(1,3,4)],summary(bae6)$fixed[10:14,c(1,3,4)])
  
  )),3)
write.csv(rs,file="Dres.csv")


#### Visualize interaction MACE ####

## Re-run model excluding non significant interaction terms
bmace <- brm(OUT_MACE ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bmi <- brm(OUT_MI ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bacm <- brm(OUT_ACM ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bcm <- brm(OUT_CM ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 
bae <- brm(OUT_AE ~ AGE + SEX + NAst + SIst + NASI + (1 + AGE + SEX + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 2000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99)) 

bmace <- brm(OUT_MACE ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bmi <- brm(OUT_MI ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bacm <- brm(OUT_ACM ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
bcm <- brm(OUT_CM ~ NAst + SIst + NASI + (1 + NAst + SIst + NASI || DID), data = dat, family = bernoulli("logit"), warmup = 1000,  iter   = 3000, chains = 3, inits  = "random", cores  = 2,control = list(adapt_delta = 0.99,max_treedepth=15)) 
save(bmace,file="bmace_hf.rdata")
save(bmi,file="bmi_hf.rdata")
save(bacm,file="bacm_hf.rdata")
save(bcm,file="bcm_hf.rdata")

load("bmace_hf.rdata")
load("bacm_hf.rdata")
load("bcm_hf.rdata")


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
bcm %>%
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
bmi %>%
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
#probs[which(probs$NA_st==2),]


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



#### Function to plot posterior distributions
    
# Posterior density
bacm2t <- ggs(bacm2)
p1<-ggplot(filter(bacm2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(bacm2)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  theme_light() +
  ylim(0,10)+
  labs(title = "All-cause mortality")+
  ylab("Posterior density")

bcm2t <- ggs(bcm2)
p2<-ggplot(filter(bcm2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(bcm2)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  theme_light() +
  ylim(0,10)+
  labs(title = "Cardiac mortality")+
  ylab("Posterior density")

bmi2t <- ggs(bmi2)
p3<-ggplot(filter(bmi2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(bmi2)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  theme_light() +
  ylim(0,10)+
  labs(title = "Myocardial infarction (MI)")+
  ylab("Posterior density")

bmace2t <- ggs(bmace2)
p4<-ggplot(filter(bmace2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(bmace2)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  theme_light() +
  ylim(0,10)+
  labs(title = "Major adverse cardiac event (MACE)")+
  ylab("Posterior density")

bae2t <- ggs(bae2)
p5<-ggplot(filter(bae2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "Regression coefficient Type D effect (NA*SI interaction)",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(bae2)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  theme_light() +
  ylim(0,10)+
  labs(title = "Any adverse event")+
  ylab("Posterior density")

pdf(file="Dposteriors.pdf",width=5)
grid.arrange(p1,p2,p3,p4,p5,ncol=1)
dev.off()


#### https://stackoverflow.com/questions/57815305/interaction-plots-for-continuous-variables-in-logistic-regression
#### https://www.stata.com/stata-news/news32-1/spotlight/



# Posterior densities CABG & PCI
bacm2t <- ggs(bcabg)
p1<-ggplot(filter(bacm2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-3, 3)) + 
  geom_vline(xintercept = summary(bcabg)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  geom_vline(xintercept = summary(bcabg)$fixed["NASI",1],
             col = "blue",
             linetype = 3) +
  theme_light() +
  ylim(0,2)+
  labs(title = "CABG")+
  ylab("Posterior density")
p1

bcm2t <- ggs(bpci)
p2<-ggplot(filter(bcm2t,
                  Parameter == "b_NASI", 
                  Iteration > 1000),
           aes(x = value))+
  geom_density(fill  = "yellow", 
               alpha = .15)+
  geom_vline(xintercept = 0, 
             col  = "darkred",
             size = .8)+
  scale_x_continuous(name   = "",
                     limits = c(-3, 3)) + 
  geom_vline(xintercept = summary(bpci)$fixed["NASI",3:4],
             col = "orange",
             linetype = 2) +
  geom_vline(xintercept = summary(bpci)$fixed["NASI",1],
             col = "blue",
             linetype = 3) +
  theme_light() +
  ylim(0,2)+
  labs(title = "PCI")+
  ylab("Posterior density")


pdf(file="Dposteriors2.pdf",width=5,height=4)
grid.arrange(p1,p2,ncol=1)
dev.off()








# Caterpillar plots
ggplot(filter(bmace2t, Parameter %in% c("b_Intercept", "b_NASI")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")





#############################################################################
#### Analysis per study   ####
##############################

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

pdf(file=paste0("forest_acm.pdf"))
forest(mal[[1]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="All-cause mortality",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[1]]$k - mal[[1]]$p),,") = ",
                                            .(formatC(mal[[1]]$QE, digits=2, format="f")),  
                                            ", p = ", .(formatC(mal[[1]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(mal[[1]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_cm.pdf"))
forest(mal[[2]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Cardiac mortality",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[2]]$k - mal[[2]]$p),,") = ",
                                             .(formatC(mal[[2]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[2]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[2]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_mi.pdf"))
forest(mal[[3]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Myocardial infarction",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[3]]$k - mal[[3]]$p),,") = ",
                                             .(formatC(mal[[3]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[3]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[3]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_cabg.pdf"))
forest(mal[[4]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),clim=c(0,10),at=0:6,main="Coronary artery bypass grafting",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[4]]$k - mal[[4]]$p),,") = ",
                                             .(formatC(mal[[4]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[4]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[4]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_pci.pdf"))
forest(mal[[5]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Percutaneous coronary intervention",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[5]]$k - mal[[5]]$p),,") = ",
                                             .(formatC(mal[[5]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[5]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[5]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_mace.pdf"))
forest(mal[[6]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Major adverse cardiac events",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[6]]$k - mal[[6]]$p),,") = ",
                                             .(formatC(mal[[6]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[6]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[6]]$I2, digits=1, format="f")), "%")))
dev.off()
pdf(file=paste0("forest_ae.pdf"))
forest(mal[[7]],transf=exp,refline=1,top=1,alim=c(0,6),xlim=c(-4,10),at=0:6,main="Any adverse events",slab=didtable[,4],xlab="Odds ratio")
text(-4, -1.7, pos=4, cex=0.75, bquote(paste("Q(",.(mal[[7]]$k - mal[[7]]$p),,") = ",
                                             .(formatC(mal[[7]]$QE, digits=2, format="f")),  
                                             ", p = ", .(formatC(mal[[7]]$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(mal[[7]]$I2, digits=1, format="f")), "%")))
dev.off()







##############################
#### Frequentist analyses ####
##############################

### Multilevel logistic regression with random study intercept ###

## Outcome: AE ##

# Continuous method
ae0<-glmer(OUT_AE ~ AGE + SEX + NAst + SIst + (1|DID), data=dat, family=binomial("logit"))
ae1<-glmer(OUT_AE ~ AGE + SEX + NAst + SIst + NA2 + SI2 + (1|DID), data=dat, family=binomial("logit"))
ae2<-glmer(OUT_AE ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1|DID), data=dat, family=binomial("logit"))
summary(ae0)
summary(ae1)
summary(ae2)
anova(ae1,ae2,test="Chisq")
nrow(model.frame(ae2))

# 2-group method
ae3<-glmer(OUT_AE ~ AGE + SEX + TYPED + (1|DID), data=dat, family=binomial("logit"))
summary(ae3)

# 4-group method
ae4<-glmer(OUT_AE ~ AGE + SEX + factor(TYPED4) + (1|DID), data=dat, family=binomial("logit"))
summary(ae4)


## Outcome: ACM ##

# Continuous method
acm0<-glmer(OUT_ACM ~ AGE + SEX + NAst + SIst + (1|DID), data=dat, family=binomial("logit"))
acm1<-glmer(OUT_ACM ~ AGE + SEX + NAst + SIst + NA2 + SI2 + (1|DID), data=dat, family=binomial("logit"))
acm2<-glmer(OUT_ACM ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1|DID), data=dat, family=binomial("logit"))
summary(acm0)
summary(acm1)
summary(acm2)
anova(acm1,acm2,test="Chisq")
nrow(model.frame(acm2))

# 2-group method
acm3<-glmer(OUT_ACM ~ AGE + SEX + TYPED + (1|DID), data=dat, family=binomial("logit"))
summary(acm3)

# 4-group method
acm4<-glmer(OUT_ACM ~ AGE + SEX + factor(TYPED4) + (1|DID), data=dat, family=binomial("logit"))
summary(acm4)


## Outcome: CM ##

# Continuous method
cm0<-glmer(OUT_CM ~ AGE + SEX + NAst + SIst + (1|DID), data=dat, family=binomial("logit"))
cm1<-glmer(OUT_CM ~ AGE + SEX + NAst + SIst + NA2 + SI2 + (1|DID), data=dat, family=binomial("logit"))
cm2<-glmer(OUT_CM ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1|DID), data=dat, family=binomial("logit"))
summary(cm0)
summary(cm1)
summary(cm2)
anova(acm1,acm2,test="Chisq")
nrow(model.frame(cm2))

# 2-group method
cm3<-glmer(OUT_CM ~ AGE + SEX + TYPED + (1|DID), data=dat, family=binomial("logit"))
summary(cm3)

# 4-group method
cm4<-glmer(OUT_CM ~ AGE + SEX + factor(TYPED4) + (1|DID), data=dat, family=binomial("logit"))
summary(cm4)

## Outcome: MACE ##

# Continuous method
mace0<-glmer(OUT_MACE ~ AGE + SEX + NAst + SIst + (1|DID), data=dat, family=binomial("logit"))
mace1<-glmer(OUT_MACE ~ AGE + SEX + NAst + SIst + NA2 + SI2 + (1|DID), data=dat, family=binomial("logit"))
mace2<-glmer(OUT_MACE ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1|DID), data=dat, family=binomial("logit"))
summary(mace0)
summary(mace1)
summary(mace2)
anova(mace1,mace2,test="Chisq")
nrow(model.frame(mace2))

# 2-group method
mace3<-glmer(OUT_MACE ~ AGE + SEX + TYPED + (1|DID), data=dat, family=binomial("logit"))
summary(mace3)

# 4-group method
mace4<-glmer(OUT_MACE ~ AGE + SEX + factor(TYPED4) + (1|DID), data=dat, family=binomial("logit"))
summary(mace4)

## Outcome: MI ##

# Continuous method
mi0<-glmer(OUT_MI ~ AGE + SEX + NAst + SIst  + (1|DID), data=dat, family=binomial("logit"))
mi1<-glmer(OUT_MI ~ AGE + SEX + NAst + SIst + NA2 + SI2 + (1|DID), data=dat, family=binomial("logit"))
mi2<-glmer(OUT_MI ~ AGE + SEX + NAst + SIst + NA2 + SI2 + NASI + (1|DID), data=dat, family=binomial("logit"))
summary(mi0)
summary(mi1)
summary(mi2)
anova(mi1,mi2,test="Chisq")
nrow(model.frame(mi2))

# 2-group method
mi3<-glmer(OUT_MI ~ AGE + SEX + TYPED + (1|DID), data=dat, family=binomial("logit"))
summary(mi3)

# 4-group method
mi4<-glmer(OUT_MI ~ AGE + SEX + factor(TYPED4) + (1|DID), data=dat, family=binomial("logit"))
summary(mi4)



## Summarize odds ratio results

# Function to get odds ratio's and confidence intervals
oci<-function(model,term){
  paste0(round(exp(summary(model)$coefficients[term,1]),2)," [",round(exp(summary(model)$coefficients[term,1]-1.96*summary(model)$coefficients[term,2]),2),", ",round(exp(summary(model)$coefficients[term,1]+1.96*summary(model)$coefficients[term,2]),2),"]")
}

# Function to get p-values
pva<-function(model,term){
  ifelse(round(summary(model)$coefficients[term,4],3)<.001,"<.001",round(summary(model)$coefficients[term,4],3))
}

# Combine results in dataframe
ores<-data.frame( AE_OR=c(oci(ae3,"TYPED"),oci(ae4,"factor(TYPED4)NA-SI-"),oci(ae4,"factor(TYPED4)NA-SI+"),oci(ae4,"factor(TYPED4)NA+SI-"),oci(ae0,"NAst"),oci(ae0,"SIst"),oci(ae1,"NA2"),oci(ae1,"SI2"),oci(ae2,"NASI")),
                  AE_p=c(pva(ae3,"TYPED"),pva(ae4,"factor(TYPED4)NA-SI-"),pva(ae4,"factor(TYPED4)NA-SI+"),pva(ae4,"factor(TYPED4)NA+SI-"),pva(ae0,"NAst"),pva(ae0,"SIst"),pva(ae1,"NA2"),pva(ae1,"SI2"),pva(ae2,"NASI")),
                  ACM_OR=c(oci(acm3,"TYPED"),oci(acm4,"factor(TYPED4)NA-SI-"),oci(acm4,"factor(TYPED4)NA-SI+"),oci(acm4,"factor(TYPED4)NA+SI-"),oci(acm0,"NAst"),oci(acm0,"SIst"),oci(acm1,"NA2"),oci(acm1,"SI2"),oci(acm2,"NASI")),
                  ACM_p=c(pva(acm3,"TYPED"),pva(acm4,"factor(TYPED4)NA-SI-"),pva(acm4,"factor(TYPED4)NA-SI+"),pva(acm4,"factor(TYPED4)NA+SI-"),pva(acm0,"NAst"),pva(acm0,"SIst"),pva(acm1,"NA2"),pva(acm1,"SI2"),pva(acm2,"NASI")),
                  CM_OR=c(oci(cm3,"TYPED"),oci(cm4,"factor(TYPED4)NA-SI-"),oci(cm4,"factor(TYPED4)NA-SI+"),oci(cm4,"factor(TYPED4)NA+SI-"),oci(cm0,"NAst"),oci(cm0,"SIst"),oci(cm1,"NA2"),oci(cm1,"SI2"),oci(cm2,"NASI")),
                  CM_p=c(pva(cm3,"TYPED"),pva(cm4,"factor(TYPED4)NA-SI-"),pva(cm4,"factor(TYPED4)NA-SI+"),pva(cm4,"factor(TYPED4)NA+SI-"),pva(cm0,"NAst"),pva(cm0,"SIst"),pva(cm1,"NA2"),pva(cm1,"SI2"),pva(cm2,"NASI")),
                  MI_OR=c(oci(mi3,"TYPED"),oci(mi4,"factor(TYPED4)NA-SI-"),oci(mi4,"factor(TYPED4)NA-SI+"),oci(mi4,"factor(TYPED4)NA+SI-"),oci(mi0,"NAst"),oci(mi0,"SIst"),oci(mi1,"NA2"),oci(mi1,"SI2"),oci(mi2,"NASI")),
                  MI_p=c(pva(mi3,"TYPED"),pva(mi4,"factor(TYPED4)NA-SI-"),pva(mi4,"factor(TYPED4)NA-SI+"),pva(mi4,"factor(TYPED4)NA+SI-"),pva(mi0,"NAst"),pva(mi0,"SIst"),pva(mi1,"NA2"),pva(mi1,"SI2"),pva(mi2,"NASI")),
                  MACE_OR=c(oci(mace3,"TYPED"),oci(mace4,"factor(TYPED4)NA-SI-"),oci(mace4,"factor(TYPED4)NA-SI+"),oci(mace4,"factor(TYPED4)NA+SI-"),oci(mace0,"NAst"),oci(mace0,"SIst"),oci(mace1,"NA2"),oci(mace1,"SI2"),oci(mace2,"NASI")),
                  MACE_p=c(pva(mace3,"TYPED"),pva(mace4,"factor(TYPED4)NA-SI-"),pva(mace4,"factor(TYPED4)NA-SI+"),pva(mace4,"factor(TYPED4)NA+SI-"),pva(mace0,"NAst"),pva(mace0,"SIst"),pva(mace1,"NA2"),pva(mace1,"SI2"),pva(mace2,"NASI"))
)

rownames(ores)<-c("Type D vs. no Type D","Type D vs. NA-SI-","Type D vs. NA-SI+","Type D vs. NA+SI-","NA","SI","NA2","SI2","NA*SI")
View(ores)  



#############################
#### Interaction models  ####
#############################

### Multilevel logistic regression with random study intercept ###

## Outcome: AE ##

aei<-glmer(OUT_AE ~ scale(AGE) + SEX + NAst + SIst + scale(AGE):NAst + scale(AGE):SIst + SEX*NAst + SEX*SIst + NA2 + SI2 + NASI + scale(AGE)*NASI + SEX*NASI + (1|DID), data=dat, family=binomial("logit"))
summary(aei) # interaction sex*NA2 & interaction age*TypeD
nrow(model.frame(aei))

## Outcome: ACM ##

acmi<-glmer(OUT_ACM ~ scale(AGE) + SEX + NAst + SIst + scale(AGE):NAst + scale(AGE):SIst + SEX*NAst + SEX*SIst + NA2 + SI2 + NASI + scale(AGE)*NASI + SEX*NASI + (1|DID), data=dat, family=binomial("logit"))
summary(acmi) # interaction sex*NA2 
nrow(model.frame(acmi))

## Outcome: CM ##

cmi<-glmer(OUT_CM ~ scale(AGE) + SEX + NAst + SIst + scale(AGE):NAst + scale(AGE):SIst + SEX*NAst + SEX*SIst + NA2 + SI2 + NASI + scale(AGE)*NASI + SEX*NASI + (1|DID), data=dat, family=binomial("logit"))
summary(cmi)
nrow(model.frame(cmi))

## Outcome: MACE ##

macei<-glmer(OUT_MACE ~ scale(AGE) + SEX + NAst + SIst + scale(AGE):NAst + scale(AGE):SIst + SEX*NAst + SEX*SIst + NA2 + SI2 + NASI + scale(AGE)*NASI + SEX*NASI + (1|DID), data=dat, family=binomial("logit"))
summary(macei)
nrow(model.frame(mii))

## Outcome: MI ##

mii<-glmer(OUT_MI ~ scale(AGE) + SEX + NAst + SIst + scale(AGE):NAst + scale(AGE):SIst + SEX*NAst + SEX*SIst + NA2 + SI2 + NASI + scale(AGE)*NASI + SEX*NASI + (1|DID), data=dat, family=binomial("logit"))
summary(mii)
nrow(model.frame(macei))


