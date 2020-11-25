install.packages("devtools")
install.packages("dplyr")
install.packages("markovchain")
# Option 1: download and install latest version from GitHub
devtools::install_github("nproellochs/ReinforcementLearning")
# Option 2: install directly from bundled archive
devtoos::install_local("ReinforcementLearning_1.0.0.tar.gz")
library(ReinforcementLearning)
library(dplyr)
library(markovchain)

# setup parameters

machine<-"PC" #PC or Mac
numberfeatures<-13
numberobsperperiod<-35193
samplelength<-5000
indicator<-1 # indicator = 1 indicates that the price minus average is taken into account indicator = 0 price only
Dmax<-1/2
Cmax<-1/2
Emin<-1/10
Emax<-1
numberquantiles<-20
timespan<-numberquantiles
# Define control object
control <- list(alpha = 0.5, gamma = 0.01, epsilon = 0.1)
# select node
node<-3
mu<-0.1

testlength<-1000
teststart<-4000 #numbers of row from first row

# load up datasets provided by Hunt

if (machine =="PC"){

names<-as.matrix(read.csv("/Users/Serge Marquie/OneDrive/table/hunt/names.csv"))
labelsmat<-read.csv(paste("/Users/Serge Marquie/OneDrive/table/hunt/hunt datasets/",names[1],".csv",sep=""), header = TRUE)
} else if (machine == "Mac"){
  names<-as.matrix(read.csv("/Users/sergemarquie/OneDrive/table/hunt/names.csv"))
  labelsmat<-read.csv(paste("/Users/sergemarquie/OneDrive/table/hunt/hunt datasets/",names[1],".csv",sep=""), header = TRUE)
}

M<-array(rep(0,nrow(names)*numberfeatures*numberobsperperiod),dim=c(nrow(names),numberobsperperiod,numberfeatures))
huntlabels<-colnames(labelsmat)

for(i in 1:nrow(names)){
  #Mtemp<-as.matrix(read.csv(paste("/Users/sergemarquie/OneDrive/table/hunt/",names[i],".csv",sep=""))
  #Mtemp<-Mtemp[(nrow(Mtemp)-35193+1):nrow(Mtemp),1:13]
  #M[i,,]<-Mtemp
  M[i,,]<-as.matrix((read.csv(paste("/Users/Serge Marquie/OneDrive/table/hunt/hunt datasets/",names[i],".csv",sep=""), header = TRUE)))
  #M[i,,]<-as.matrix((read.csv(paste("/Users/sergemarquie/OneDrive/table/hunt/hunt datasets/",names[i],".csv",sep=""), header = TRUE)))
  
}



# price series
price1<-as.matrix(as.numeric(M[node,,3]))
price1[is.na(price1)]<-0

# computation of average price
average<-matrix(0,nrow=nrow(price1),ncol=1)
average[nrow(price1)]<-price1[nrow(price1)]
for (i in 1:(nrow(price1)-1)){
  average[nrow(price1)-i]<-mu*price1[nrow(price1)-i]+(1-mu)*average[nrow(price1)-i+1]
}


#compute new price matrix fullprice [price,averageprice,price - average price]
newprice<-as.matrix(price1-indicator*average)
quantile<- ntile(newprice[,1],numberquantiles)
newprice<-as.matrix(cbind(newprice,quantile))
fullprice<-as.matrix(cbind(average,newprice))
fullprice<-as.matrix(cbind(price1,fullprice))


#compute reference price for each quantile as the minimum price in the quantile
pricestates<-matrix(0,nrow=numberquantiles,ncol=1)
for (i in 1:numberquantiles){
  pricestates[i]<-min(newprice[newprice[,2]==i,1])
}


# define states
#s1 is quantile number
#s2 is energy in percentage of max energy level

s1<-matrix(c(1:timespan), ncol=1) #time
s2<-matrix(c(1/10,2/10,3/10,4/10,5/10,6/10,7/10,8/10,9/10,10/10), ncol=1) #energy

#for (i in 0:nrow(s1)){
#  for (j in 0:nrow(s2)){
#    assign(paste("s",i,j,sep=""),matrix(c(s1[i],s2[j]),ncol=1))
#  }
#}
states<-matrix("",nrow=nrow(s1)*nrow(s2),ncol=1)
for (i in 0:(nrow(s1)-1)){
  for (j in 0:(nrow(s2)-1)){
    states[as.numeric(paste(i,j,sep=""))+1]<-as.character(paste("s",i,j,sep=""))
  }
}

# create markov chain and transation matrix
mcFit <- markovchainFit(data=quantile)
TM<-as.matrix((mcFit[[1]])@transitionMatrix)

# generate sequence of 1 using transition matrix and initial state
outs <- markovchainSequence(n = 1, markovchain = mcFit[["estimate"]], t0 = "6")


#define action
actions<-c("charge", "discharge", "hold")


#create charge and discharge functions
discharge<-function(el){
  x<-min(Dmax, el-Emin)
  return(x)
}
charge<-function(el){
  x<-min(Cmax, Emax-el)
 return(x)
}

# define reward/next state function sat
# indicator at 1 function takes into account average, indicator at 0 doesn't take into account average


env<-function(state, action){
  next_state<-matrix(0,nrow=2,ncol=1)
  reward<-0
  State<-state
  Action<-action
  if(nchar(State) == 3){
  State2<-s2[as.numeric(substr(State,3,3 ))+1] #energy
  State1<-s1[as.numeric(substr(State,start=2,stop=2 ))+1] #time
  } else if (nchar(State) == 4){
    State2<-s2[as.numeric(substr(State,4,4 ))+1] #energy
    State1<-s1[as.numeric(substr(State,start=2,stop=3 ))+1] #time
  } else if (nchar(State) == 5){
    State2<-s2[as.numeric(substr(State,5,5 ))+1] #energy
    State1<-s1[as.numeric(substr(State,start=2,stop=4 ))+1] #time
  } else if (nchar(State) == 6){
    State2<-s2[as.numeric(substr(State,6,6 ))+1] #energy
    State1<-s1[as.numeric(substr(State,start=2,stop=5 ))+1] #time
  } else if (nchar(State) == 7){
    State2<-s2[as.numeric(substr(State,7,7 ))+1] #energy
    State1<-s1[as.numeric(substr(State,start=2,stop=6 ))+1] #time
  } else if (nchar(State) == 8){
    State2<-s2[as.numeric(substr(State,8,8 ))+1] #energy
    State1<-s1[as.numeric(substr(State,start=2,stop=7 ))+1] #time
  }
  
  #State1<-State[1]
  #State2<-State[2]
  
  if (Action =="charge"){
    reward<--(pricestates[State1])*charge(State2)
    next_state[2]<-round((State2+charge(State2))*10)/10
  } else if (Action =="discharge"){
    reward<-(pricestates[State1])*discharge(State2)
    next_state[2]<-round((State2-discharge(State2))*10)/10
        } else if (Action =="hold"){
      reward<-0
      next_state[2]<-State2
        }
 next_state[1] <- as.character(as.numeric(markovchainSequence(n = 1, markovchain = mcFit[["estimate"]], t0 = State1))-1)
  
  
  #ii<-which(s1[]==next_state[1])
  ii<-next_state[1]
  jj<-which(s2[]==next_state[2])-1
  
  
  out<-list(NextState=paste("s",as.character(ii),as.character(jj),sep=""), Reward=reward)
  return(out)
  
}



# Sample N = 1000 random sequences from the environment
Data <- sampleExperience(N = samplelength, 
                         env = env, 
                         states = states, 
                         actions = actions,
                         control=control)
head(Data)


# Pass learning parameters to reinforcement learning function
model <- ReinforcementLearning(data = Data,
                               s = "State",
                               a = "Action",
                               r = "Reward",
                               s_new = "NextState",
                               iter = 30, 
                               control = control)

# Compute policy
results<-computePolicy(model)
# Print state-action table
print(model)
# Print summary statistics
#summary(model)

results<-as.matrix(results)

statenames<-as.matrix(rownames(results))

pricequantile<-matrix(0,nrow=nrow(statenames),ncol=1)
energylevel<-matrix(0,nrow=nrow(statenames),ncol=1)
actionstep<-matrix(0,nrow=nrow(statenames),ncol=1)

for (i in 1:nrow(statenames)){
  if(nchar(statenames[i]) == 3){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],3,3 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=2 ))+1]) #time
    actionstep[i]<-results[i]
  } else if (nchar(statenames[i]) == 4){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],4,4 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=3 ))+1]) #time
    actionstep[i]<-results[i]
  } else if (nchar(statenames[i]) == 5){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],5,5 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=4 ))+1]) #time
    actionstep[i]<-results[i]
  } else if (nchar(statenames[i]) == 6){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],6,6 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=5 ))+1]) #time
    actionstep[i]<-results[i]
  } else if (nchar(statenames[i]) == 7){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],7,7 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=6 ))+1]) #time
    actionstep[i]<-results[i]
  } else if (nchar(statenames[i]) == 8){
    energylevel[i]<-s2[as.numeric(substr(statenames[i],8,8 ))+1] #energy
    pricequantile[i]<-as.numeric(s1[as.numeric(substr(statenames[i],start=2,stop=7 ))+1]) #time
    actionstep[i]<-results[i]
  }
}

fullresults<-cbind(pricequantile,energylevel)
fullresults<-cbind(fullresults,actionstep)
ofr<-fullresults[order(as.numeric(fullresults[,1]),fullresults[,2],decreasing=FALSE),]



oneshotprofit<-function(energylevel, action, pricelevel){
  
  Action<-action
  Price<-pricelevel
  Energy<-energylevel
  
  if (Action =="charge"){
    reward<-(-Price)*charge(Energy)
    next_state<-round((Energy+charge(Energy))*10)/10
  } else if (Action =="discharge"){
    reward<-(Price)*discharge(Energy)
    next_state<-round((Energy-discharge(Energy))*10)/10
  } else if (Action =="hold"){
    reward<-0
    next_state<-Energy
  }
  
  out<-list(En = next_state, Reward=reward)
  return(out)
  
}


startingstate<-c(teststart,0)
trading<-matrix(0,nrow=testlength, ncol=5)
trading[,1]<-fullprice[(teststart-testlength+1):(teststart),4]
trading[,5]<-fullprice[(teststart-testlength+1):(teststart),1]
trading[1,2]<-Emin
trading<-matrix(trading,ncol=5)

for (i in 1:(testlength-1)){
  state<-paste("s",as.character(as.numeric(trading[i,1])-1),as.character(which(s2==trading[i,2])-1),sep="")
  action<-results[(which(statenames==state))]
  trading[i+1,2]<-as.numeric(oneshotprofit(as.numeric(trading[i,2]),action,as.numeric(trading[i,5])))[1]
  trading[i,3]<-as.numeric(oneshotprofit(as.numeric(trading[i,2]),action,as.numeric(trading[i,5])))[2]
  trading[i,4]<-action
}



sum(as.numeric(trading[,3]))
#plot(ofr[1:500,2],type="l")
#plot(energylevel,type="l")
plot(trading[,2],type="l")

