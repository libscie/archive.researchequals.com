d<-read.csv2("67 games & 8 strategies.csv")

for (i in 7:14){
  d[,i]<-as.numeric(d[,i])
}

mean(cor(d[,7:14])[upper.tri(cor(d[,7:14]))])
min(cor(d[,7:14])[upper.tri(cor(d[,7:14]))])
max(cor(d[,7:14])[upper.tri(cor(d[,7:14]))])
mean(cor(d[d$sd>0,7:14])[upper.tri(cor(d[d$sd>0,7:14]))])



d2<-d[order(d$Nash),]


pdf("Figure 2 67 games.pdf")
par(mar=c(6, 5, 4, 6.6) + 0.1,xpd=T)

plot(d2$Nash,ylab="Probability of Cooperation",xlab="Games (ordered by Nash value)",cex.lab=2)
points(1:67,d2$K.index,pch=2)
points(1:67,d2$selfish.superrational,pch=3)
points(1:67,d2$collective.maximizer,pch=4)
points(1:67,d2$individualist.heuristic,pch=5)
points(1:67,d2$collectivist.heuristic,pch=6)
points(1:67,d2$selfish.projection,pch=7)
points(1:67,d2$prosocial.superrational,pch=8)

legend("topright", inset=c(-0.3,0), bty="n",legend=c("Nash","K-Ind.","Self-Superr.",
                                                     "Col. Max.","Ind. Heur.",
                                                     "Col. Heur.","Self-Proj.","Prosoc.-Superr."),
       pch=c(1:8),cex=.8)
dev.off()
