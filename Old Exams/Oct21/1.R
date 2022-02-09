library(bnlearn)
library(gRain)

data("asia")
net <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
net <- bn.fit(net, asia, method="bayes")


data <- matrix(NA, 1000, length(net))
for(i in 1:1000){
  a<-sample(1:2,1,prob=net$A$prob)
  s<-sample(1:2,1,prob=net$S$prob)
  t<-sample(1:2,1,prob=net$T$prob[, a])
  
  l<-sample(1:2,1,prob=net$L$prob[, s])
  b<-sample(1:2,1,prob=net$B$prob[, s])
  
  e<-sample(1:2,1,prob=net$E$prob[, l, t])
  d<-sample(1:2,1,prob=net$D$prob[, b, e])
  
  x<-sample(1:2,1,prob=net$X$prob[, e])
  
  data[i,]<-c(a,s,t,l,b,d,e,x)
}

foo <- data[which(data[, 6]==2), 2]
table(foo) / length(foo)

net <- as.grain(net)
net <- compile(net)
net <- setEvidence(net,nodes=c("D"), states=c("yes"))
querygrain(net, c("S"))


# ---