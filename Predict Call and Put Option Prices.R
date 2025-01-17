#####   Load the library needed     #####
library(OptionPricing)
library(VarianceGamma)
library(tseries)
library(moments)


#####   Import Stock Close Price Data     #####
Return <- read.csv("D:\\AMD.csv")
head(Return)
#Calculating log returns
Return <- diff(log(Return$Close))
sum(is.na(Return))


#####   Data Visualization    #####
#Line plot
plot(Return, type = "l", main = "AMD Return")
#Normal qq plot
qqnorm(Return, main = "AMD Return")
qqline(Return,col = "red")
#Histogram and distribution plot 
hist(Return, prob = TRUE, title = "AMD Return")
lines(density(Return), lwd = 2, col = "red")


#####   Bowman Shelton Normality Test    #####
BowmanShelton <- function(Data){
  BS=length(Data)*(((skewness(Data)^2)/6)+(((kurtosis(Data)-3)^2)/24))
  
  print(cbind(BowmanShelton=BS,
              Chi_Square=qchisq(0.05, 2,lower.tail = F),
              PValue=pchisq(BS, 2,lower.tail = F)))
}
BowmanShelton(Return)


#####   Skewness, kurtosis, mean, and variance    #####
round(skewness(Return), 5)
round(kurtosis(Return), 5)
round(mean(Return), 5)
round(var(Return), 7)


#####   Variance Gamma Distribution Test    #####
vgFit(Return)
XX=pvg(Return, vgC = 0, sigma = 1, theta = 0, nu = 1,
       #Substitute nilai vgC, sigmavg, theta, nu
       param = c(vgC = -1.4465, sigmavg = 0.28951, theta = 1.4568, nu = 0.1184))
z=ecdf(Return)
YY=z(Return)
KS = YY-XX
#D for Kolmogorov-Smirnov Test
ksvg=max(KS) 
#P-Value for Kolmogorov-Smirnov
critvaluevg=1.36/sqrt(length(XX)) 
#Print the Result
cbind(D=ksvg,PValue=critvaluevg)

#Option Price Simulation with Varian Gamma Distribution
#Substitute sigmaVG, thetaVG, nuVG, omegaVG
sigmaVG = 0.2895
thetaVG = 1.4568
nuVG = 0.1184 
omegaVG = (1/nuVG)*log(1-thetaVG*nuVG-0.5*sigmaVG^2*nuVG)

#Variance Gamma Simulation
#Call
VGCall <- function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
               sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  w<-(z-K)
  h<-pmax(w,0)
  m<-mean(h)
  call_VG<-m*exp(-r*T)
  callVG<-c()
  for(i in 1:n){
    callVG[i]<-w[i]*exp(-r*T)
  }
  se_callVG<-sd(callVG)/sqrt(n)
  output<-list(price_call=call_VG, sterr_call=se_callVG)
  return(output)
}

#Counting T
#Expire date - stock's last data date
T = as.numeric(as.Date("2023-01-13")-as.Date("2022-12-09"))/365
T

#Simulation results
#Substitute K dan s0 in call option
VGCall(1000000, T = 0.09589041, r = 0.0357, omega = omegaVG, 
       sigmavg = sigmaVG, nu = nuVG, theta = thetaVG, K = 65, s0 = 68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(6.85-6.419053)/6.85

#Put
VGPut <- function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
               sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  w<-(K-z)
  h<-pmax(w,0)
  m<-mean(h)
  put_VG<-m*exp(-r*T)
  putVG<-c()
  for(i in 1:n){
    putVG[i]<-w[i]*exp(-r*T)
  }
  se_putVG<-sd(putVG)/sqrt(n)
  output<-list(price_put=put_VG, sterr_put=se_putVG)
  return(output)
}
VGPut(1000000, T=0.09589041, r=0.0357, omega=omegaVG, 
      #substitute K and s0 for put option
      sigmavg=sigmaVG, nu=nuVG, theta=thetaVG, K=80, s0=68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(11.33-13.41988)/11.33


#####     Variance Gamma Antithetic Variate Reduction Simulation     #####
#Call
VG_AVRCall <- function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z1<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
                sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  z2 <- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
                 sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*(-rnorm(n,0,1)))
  w1<-((z1-K)+(z2-K))/2
  h1<-pmax(w1,0)
  m1<-mean(h1)
  call_VG_AVR<-m1*exp(-r*T)
  
  callVGAVR<-c()
  for(i in 1:n){
    callVGAVR[i]<-w1[i]*exp(-r*T)
  }
  se_callVG_AVR<-sd(callVGAVR)/sqrt(n)
  output<-list(price_call=call_VG_AVR, sterr_call=se_callVG_AVR)
  return(output)
}
VG_AVRCall(1000000, T=0.09589041, r=0.0357, omega=omegaVG, 
           #substitute K and s0 for call option
           sigmavg=sigmaVG, nu=nuVG, theta=thetaVG, K=65, s0=68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(6.85-5.462215)/6.85

#Put
VG_AVRPut <- function(n, T, r, omega, sigmavg, nu, theta,K,s0){
  z1<- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
                sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*rnorm(n,0,1))
  z2 <- s0*exp((r+omega)*T + theta*rgamma(n, shape=T/nu, scale = nu) + 
                 sigmavg*sqrt(rgamma(n, shape=T/nu, scale = nu))*(-rnorm(n,0,1)))
  w1<-((K-z1)+(K-z2))/2
  h1<-pmax(w1,0)
  m1<-mean(h1)
  put_VG_AVR<-m1*exp(-r*T)
  
  putVGAVR<-c()
  for(i in 1:n){ 
    putVGAVR[i]<-w1[i]*exp(-r*T)
  }
  se_putVG_AVR<-sd(putVGAVR)/sqrt(n)
  output<-list(price_put=put_VG_AVR, sterr_put=se_putVG_AVR)
  return(output)
}
VG_AVRPut(1000000, T = 0.09589041, r=0.0357, omega=omegaVG, 
          #substitute K and s0 for put option
          sigmavg=sigmaVG, nu=nuVG, theta=thetaVG, K=80, s0=68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(11.33-12.35083)/11.33


#####     Black-Scholes Option Price     #####
#Call
sigma = sqrt(252*0.0015268) #sqrt(252*variance return) 
#Substitute K dan s0
BS_EC(K = 65, r = 0.0357, sigma = 0.6202851, T = 0.09589041, S0 = 68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(6.85-7.23176822)/6.85

#Put
#Substitute K dan s0
BS_EP(K = 80, r = 0.0357, sigma = 0.6202851, T = 0.09589041, S0 = 68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(11.33-12.89009304)/11.33


#####     Black-Sholes Option Price with Gram-Charlier Expansion     #####
#Call
GramCharlierCall <- function(x,K,r,sigma,T,S0){
  BS_Call=BS_EC(K=65, r = 0.0357, sigma = 0.6202851, T = 0.09589041, S0 = 68.59)
  CBS=BS_Call['price']
  d1 <- (log(S0/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  Q3 = 1/(3*2)*S0*sigma*sqrt(T)*((2*sigma*sqrt(T)-d1)*dnorm(d1)-sigma*sigma*T*pnorm(d1))
  Q4 = 1/(4*3*2)*S0*sigma*sqrt(T)*((d1^2-1-3*sigma*sqrt(T)*(d1-sigma*sqrt(T)))*dnorm(d1)+sigma^3*T^(3/2)*pnorm(d1))
  CGC <- CBS + skewness(x)*Q3 + (kurtosis(x)-3)*Q4
  return(CGC)
}
GramCharlierCall(x=Return,K=65, r = 0.0357, sigma = 0.6202851, T = 0.09589041, S0 = 68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(6.85-7.100853)/6.85

#Put
GramCharlierPut <- function(x,K,r,sigma,T,S0){
  BS_Put=BS_EP(K=80, r = 0.0357, sigma = 0.6202851, T = 0.09589041, S0 = 68.59)
  PBS=BS_Put['price']
  d1 <- (log(S0/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  Q3 = 1/(3*2)*S0*sigma*sqrt(T)*((2*sigma*sqrt(T)-d1)*dnorm(d1)-sigma*sigma*T*pnorm(d1))
  Q4 = 1/(4*3*2)*S0*sigma*sqrt(T)*((d1^2-1-3*sigma*sqrt(T)*(d1-sigma*sqrt(T)))*dnorm(d1)+sigma^3*T^(3/2)*pnorm(d1))
  PGC <- PBS + skewness(x)*Q3 + (kurtosis(x)-3)*Q4
  return(PGC)
}
GramCharlierPut(x=Return,K=80, r = 0.0357, sigma = 0.6202851, 
                T = 0.09589041, S0 = 68.59)

#Counting MAE
# |market price - simulation price| /market price
abs(11.33-12.84416 )/11.33