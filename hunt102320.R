install.packages("pcalg")
library("pcalg")
install.packages("lmtest")
library("lmtest")
install.packages("fpp")
library("fpp")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")

library("RBGL")
library("Rgraphviz")


data("gmG")

hunt<-as.matrix(read.csv("/Users/sergemarquie/OneDrive/table/hunt/hunt_hoefsrd.csv"))
hunt<-as.matrix(read.csv("/Users/Serge Marquie/OneDrive/table/hunt/hunt_hoefsrd.csv"))

data_hunt<-matrix(as.numeric(as.matrix(hunt[,3:ncol(hunt)])),nrow=nrow(hunt))

data_hunt.diff<-diff(data_hunt,lag=1,differences=1)
data_hunt.diff[is.na(data_hunt.diff)]<-0

data_hunt[is.na(data_hunt)]<-0
data_hunt.seas_adj<-matrix(0,nrow=nrow(data_hunt), ncol=ncol(data_hunt))

for(j in 1:ncol(data_hunt.diff)){
  ts_data_hunt<-ts(as.matrix(data_hunt[,j]), frequency = 35015, start =2018)
dec_ts_data_hunt<-decompose(ts_data_hunt,"additive")  
data_hunt.seas_adj[,j]<-data_hunt[,j]-dec_ts_data_hunt$seasonal}

write.csv(data_hunt.seas_adj,"/Users/sergemarquie/OneDrive/table/hunt/data_hunt.seas_adj.csv")
write.csv(data_hunt.seas_adj,"/Users/Serge Marquie/OneDrive/table/hunt/data_hunt.seas_adj.csv")


data_hunt.seas_adj.diff<-diff(data_hunt.seas_adj,lag=1,differences=1)

# alternative way to make data stationary by taking percentage differences rather than straight differences

install.packages("quantmod")
library(quantmod)
data_hunt.seas_adj.perc_diff<-matrix(0,nrow=nrow(data_hunt.seas_adj),ncol=ncol(data_hunt.seas_adj))

for (i in (1:ncol(data_hunt.seas_adj))){
data_hunt.seas_adj.perc_diff[,i]<-Delt(data_hunt.seas_adj[,i],k=1,type="arithmetic")}

data_hunt.seas_adj.perc_diff<-as.matrix(data_hunt.seas_adj.perc_diff[2:nrow(data_hunt.seas_adj.perc_diff),])
# compute price vol from historical prices

volperiod<-30 #number of days used to compute price volatility
vol<-matrix(0,nrow=nrow(data_hunt.seas_adj.perc_diff)-volperiod,ncol=1)

for (i in (volperiod+1):nrow(data_hunt.seas_adj.perc_diff)){
  vol[i-volperiod]<-var(data_hunt.seas_adj.perc_diff[(i-volperiod):i])
}



data_hunt.seas_adj.perc_diff.vol<-cbind(Delt(vol,k=1,type="arithmetic"),data_hunt.seas_adj.perc_diff[(volperiod+1):nrow(data_hunt.seas_adj.perc_diff),])





# define data on which analysis executed

hunt_data_matrix<-data_hunt.seas_adj.perc_diff[1:30000,]
hunt_data_matrix_scaled<-scale(hunt_data_matrix,center = TRUE, scale=TRUE)



suffStat1<-list(C = cor(hunt_data_matrix), n = nrow(hunt_data_matrix))
#suffStat<-list(C = cor(gmG8$x), n = nrow(gmG8$x))

pc.hunt_data_matrix<- pc(suffStat1, indepTest = gaussCItest, p=ncol(hunt_data_matrix), alpha=0.01)
#pc.gmG<- pc(suffStat, indepTest = gaussCItest, p=ncol(gmG8$x), alpha=0.01)


skel.hunt_data_matrix<-skeleton(suffStat1, indepTest = gaussCItest, p=ncol(hunt_data_matrix), alpha = 0.01)
par(mfrow = c(1,2))
plot(data_hunt.diff, main = ""); plot(skel.hunt_data_matrix, main = "")

par(mfrow = c(1,2))
plot(hunt_data_matrix, main = ""); plot(pc.hunt_data_matrix, main = "")

#causality matrix
CM<-matrix(0,ncol=ncol(hunt_data_matrix),nrow=ncol(hunt_data_matrix))
for (i in 1:ncol(hunt_data_matrix)){
  for (j in 1:ncol(hunt_data_matrix)){
    CM[i,j]<-mean(ida(i,j,cov(hunt_data_matrix),pc.hunt_data_matrix@graph))
  }
}

write.csv(CM,"/Users/sergemarquie/OneDrive/table/hunt/CM.csv")
write.csv(CM,"/Users/Serge Marquie/OneDrive/table/hunt/CM.csv")


GT<-matrix(0,ncol=ncol(hunt_data_matrix),nrow=ncol(hunt_data_matrix))
for (i in 1:ncol(hunt_data_matrix)){
  for (j in 1:ncol(hunt_data_matrix)){
    if (i==j){GT[i,j]=0} else {
    GT[i,j]<-grangertest(hunt_data_matrix[,i],hunt_data_matrix[,j],order=1)[2,3]}
  }
}

write.csv(GT,"/Users/sergemarquie/OneDrive/table/hunt/GT_hunt2.csv")
write.csv(GT,"/Users/Serge Marquie/OneDrive/table/hunt/GT_hunt3.csv")


ida(1, c(2,3,4,5,6), cov(data_hunt.diff), pc.data_hunt.diff@graph)
ida(1, 6, cov(gmG8$x), pc.gmG@graph)

install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(caret)
library(randomForest)



rollingtrainingsetdim<-1000
timelag<-10 #(needs to be >2)

trainingset<-hunt_data_matrix_scaled

#analysis starting point, starting on the last date available
startingpoint<-nrow(trainingset)
lengthoftest<-1000
forecast<-matrix(0,nrow=lengthoftest,ncol=1)
reference <-matrix(0,nrow=lengthoftest,ncol=1)

# testing is only on observation 1, training is on rollingtrainingsetdim interval before i-1
for (i in (nrow(trainingset)-lengthoftest+1):nrow(trainingset)){
  Y<-matrix(trainingset[(i-1-rollingtrainingsetdim):(i-1),1],ncol=1)
  X<-trainingset[(i-2-rollingtrainingsetdim):(i-2),2:ncol(trainingset)] #selects all independent variables at time t-1 to model dependent variable at time t
  if (timelag >=2){
  for (j in 2:timelag){
  X<-cbind(X,trainingset[(i-j-rollingtrainingsetdim-1):(i-j-1),2:ncol(trainingset)])
  }
  }else {}
  hunt.rf<-randomForest(y=as.vector(Y),x=X, ntree=500, type = "regression" )
  
  testingset<-trainingset[(i-timelag):i,]
  Ytesting<-as.matrix(testingset[timelag+1,1])
  Xtesting<-matrix(testingset[timelag,2:ncol(testingset)],nrow=1)
  if (timelag >=2){
    for (j in 2:timelag){
      Xtesting<-matrix(cbind(Xtesting,matrix(testingset[(timelag-j+1),2:ncol(trainingset)],nrow=1)),nrow=1)
    }
  }else {}
  
  Yforecast<-predict(hunt.rf, Xtesting ,type="response" )
  forecast[i-nrow(trainingset)+lengthoftest]<-Yforecast
  reference[i-nrow(trainingset)+lengthoftest]<-Ytesting[1,1]
}

accuracy <-(sum(sign(forecast*reference))+lengthoftest)/(2*lengthoftest)

# generalized random forest

install.packages("grf")
library(grf)

grftraining<-data_hunt.seas_adj.perc_diff[(nrow(data_hunt.seas_adj.perc_diff)-1000):nrow(data_hunt.seas_adj.perc_diff)]
Y<-grftraining[,1]
k<-4
W<-grftraining[,k+1]
X<-cbind(grftraining[,2:k],grftraining[,(k+2):ncol(grftraining)])
hunt.forest<-causal_forest(X,Y,W)

# Estimate treatment effects for the training data using out-of-bag prediction.
hunt.hat.oob <- predict(hunt.forest)
hist(hunt.hat.oob$predictions)

X.test<- ??

# Estimate treatment effects for the test sample.
tau.hat <- predict(hunt.forest, X.test)
hist(tau.hat$predictions)
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 2)
