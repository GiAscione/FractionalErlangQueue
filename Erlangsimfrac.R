#Function for the simulation of the fractional Erlang queue:
#In input:
#n is the number of events we want to simulate;
#N0 is the initial state of the queue;
#S0 is the initial phase of the queue;
#k is the shape parameter of the service system;
#lambda is the parameter of the input;
#mu is the rate of the service system;
#nu is the fractional order;
#inttime is the time interval for the discretization of the process.
#In output: the function gives a matrix M in output whose first row is the queue state process, while the second row is the current phase process.
#The script needs as source:
#the function "MLgen", provided in another script of this repository;
#the function "rstable", contained in the library "stabledist".
Erlangsimfractional<-function(n,N0,S0,k,lambda,mu,nu,inttime){
  #Determination of the rate
  rate<-lambda+k*mu
  #Simulation of the first inter-event time (depending on the fact that the queue is empty or not)
  if (N0==0){
    IntEv<-MLgen(1,nu,lambda)
  }else{
    IntEv<-MLgen(1,nu,rate)
  }
  #Initialization of the calendar vector
  Calendar<-vector(length=n)
  Calendar[1]<-IntEv[1]
  #Inizialization of the queue state and queue phase processes
  i<-2
  N<-vector(length=1)
  S<-vector(length=1)
  N[1]<-N0
  S[1]<-S0
  j<-1
  #Starting the simulation
  while(j<n+1){
    N<-c(N,0)
    S<-c(S,0)
    if((i-1)*inttime<Calendar[j]){
      #If a new event has not already occured, fix the values.
      N[i]<-N[i-1]
      S[i]<-S[i-1]
    }else{
      #If a new event has occured
      j<-j+1
      if(N[i-1]==0){
        N[i]=1
        S[i]=k
      }else{
        #If the queue is not empty, decide what event has to happen
        U<-runif(1)
        prob<-lambda/rate
        if(U<prob){
          #If someone joins the queue
          N[i]<-N[i-1]+1
          S[i]<-S[i-1]
        }else{
          if(S[i-1]>1){
            #If someone ends a phase that is not his last phase
            N[i]<-N[i-1]
            S[i]<-S[i-1]-1
          }else{
            #If someone ends his last phase
            N[i]<-N[i-1]-1
            if(N[i]==0){
              #and he was the last user
              S[i]<-0
            }else{
              #and he was not the last user
              S[i]<-k
            }
          }
        }
      }
      #Simulate the next inter-event time
      if (N[i]==0){
        IntEv<-MLgen(1,nu,lambda)
      }else{
        IntEv<-MLgen(1,nu,rate)
      }
      #Update the Calendar vector
      Calendar[j]<-Calendar[j-1]+IntEv[1]
    }
    i<-i+1
  }
  #Create the output matrix
  numcol<-i-1
  M<-matrix(nrow=2,ncol=numcol)
  for( j in c(1:numcol)){
    M[1,j]<-N[j]
    M[2,j]<-S[j]
  }
  return(M)
}
