#Setting the parameters
nu<-0.75
n<-50
N0<-0
S0<-0
k<-2
lambda<-4
mu<-5
inttime<-0.01
#Using the function
M<-Erlangsimfractional(n,N0,S0,k,lambda,mu,nu,inttime)
#Constructing the time-step vector for the plot
num<-ncol(M)
Time<-c(0:(num-1))
Time<-Time*inttime
#Constructing the queue-state vector for the plot
N<-vector(length=num)
for (i in c(1:num)){
  N[i]<-M[1,i]
}
#Plot of the queue-state process
plot(Time,N,type="s",xlab="t",ylab="")
#Extracting the jump chain
Ncat<-vector(length=1)
j<-2
Ncat[1]<-0
i<-2
Newn<-length(N)
while(i<n-2 && j<Newn){
  Ncat<-c(Ncat,0)
  while(N[j]-N[j-1]==0){
    j<-j+1
  }
  Ncat[i]<-N[j]
  j<-j+1
  i<-i+1
}
#Plotting the jump chain
plot(c(0:(length(Ncat)-1)),Ncat,type="s",xlab="n",ylab="")
