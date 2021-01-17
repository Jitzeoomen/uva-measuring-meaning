install.packages("TraMineR")
library("TraMineR")
data("mvad")
help(mvad)
mvad.labels <- c("Employment", "Further education", "Higher education", "Joblessness", "School", "Training")
mvad.scode <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, states=mvad.scode, labels=mvad.labels)
par(mfrow=c(2,2))
  {
  seqiplot(mvad.seq, with.legend=F, main = "Index plot (first ten sequences)")
  seqfplot(mvad.seq,pbarw=T, with.legend=F,main="Sequence frequency plot")
  seqdplot(mvad.seq, with.legend=F,main="State distribution plot")
  seqlegend(mvad.seq,cex = 0.5)
}


par(mfrow=c(1,1))
{Entropy<-seqstatd(mvad.seq)$Entropy
  plot(Entropy, main="Entropy of the state distribution",col="black",xlab="Time(months)",ylab="Entropy",type='l')
}

submat<-seqsubm(mvad.seq,method="TRATE")
dist.om1<-seqdist(mvad.seq, method="OM", indel=1, sm=submat)

library(cluster)
clusterward1<-agnes(dist.om1,diss=TRUE,method="ward")
par(mfrow=c(1,1))
plot(clusterward1,which.plot=2)
cluster3<-cutree(clusterward1,k=3)
cluster3fac<-factor (cluster3, labels= c("Type 1", "Type 2", "Type 3"))
seqdplot(mvad.seq,group=cluster3fac)

