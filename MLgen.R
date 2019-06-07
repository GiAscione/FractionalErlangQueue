#Function for the simulation of a Mittag-Leffler random variable.
#In input:
#n is the number of ML random variable one wants to simulate;
#nu is the fractional order of the random variable;
#lambda is the parameter of the random variable.
#In output: a vector of length n contanining the simulated ML random variables.
#The script needs as source the function "rstable" contained in the library "stabledist"
MLgen<-function(n,nu,lambda){
  #Simulate n exponential random variables of parameter lambda
  Z1<-rexp(n,lambda)
  #Set the parameters for the simulation of the stable random variable
  gamma<-(cos(pi*nu/2))^(1/nu)
  #Simulate n stable random variables of order nu
  Z2<-rstable(n,nu,1,gamma,0,pm=1)
  #Produce the simulated ML random variable
  Z<-vector(length=n)
  for (i in c(1:n)){
    Z[i]<-(Z1[i]^(1/nu))*Z2[i]
  }
  return(Z)
}