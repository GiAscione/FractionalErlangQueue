#Setting the parameters
nu<-0.75
n<-50
N0<-0
S0<-0
k<-2
lambda<-4
mu<-5
inttime<-0.01
#Using the function to simulate a classical Erlang queue
M<-Erlangsimclassic(n,N0,S0,k,lambda,mu,inttime)
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
#Simulating a stable subordinator
gamma<-(cos(pi*nu/2))^(1/nu)
X<-vector(length=num)
X[1]<-0
i<-2
while(i<=num){
  print(i)
  Z<-rstable(1,nu,1,gamma,0,pm=1)
  Z<-(inttemp^(1/alpha))*Z
  X[i]<-X[i-1]+Z
  i<-i+1
}
#Plot of the time-changed queue-state process
plot(X,N,type="s",xlab="t",ylab="")
