# creation of all symmetric games

R<-S<-T<-P<-c(1,2,3,4)

df<-as.data.frame(expand.grid(S,R,P,T))
#df<-rbind(df,c(0,0,0,0))

colnames(df)<-c("S","R","P","T")

df$n_unique<-NA
df$n_1<-NA
df$n_2<-NA
df$n_3<-NA
df$n_4<-NA
for(i in 1:nrow(df)){df$n_unique[i]<-length(unique(as.numeric(df[i,1:4])))}
for(i in 1:nrow(df)){df$n_1[i]<-sum(df[i,1:4]==1)}
for(i in 1:nrow(df)){df$n_2[i]<-sum(df[i,1:4]==2)}
for(i in 1:nrow(df)){df$n_3[i]<-sum(df[i,1:4]==3)}
for(i in 1:nrow(df)){df$n_4[i]<-sum(df[i,1:4]==4)}


df<-df[
  (df$n_unique==4&(df$T==4|df$R==4))|   #all symmetric games without ties: 12 
  (df$n_unique==3&df$n_1>0&(df$T==4|df$R==4))|   #should be 22: include also the other middle value = 44
  ((df$n_1==2&df$n_4==2)&(df$T==4|df$R==4))| # double ties
  ((df$n_1==3&df$n_4==1|df$n_4==3&df$n_1==1)&(df$T==4|df$R==4)),] # triples from Bruns, additionally creates 4144 and 1444
     
write.csv(df[,1:4],"all symmetric games 67.csv",row.names = FALSE)
