### Basic Version of Promotion Effects - Neslin, van Heerde (2009) w/o any stockpiling other effects

library(bayesm)
library(data.table)
library(magrittr)
library(ggplot2)
library(data.table)

#Function to create price matrix - from dataset orangejuice from bayesm package
PriceMatrix <- function(){
  data("orangeJuice")
  oj = data.table(orangeJuice$yx)
  
  # we only use 64 oz package sizes and multiple price by 64 becasue raw prices are per oz.
  prices64 = oj[store == 5 & brand == 1, .(week = 1:.N,
                                           p_trp = price1, p_fn = price3, 
                                           p_tr = price4, p_mm = price5, 
                                           p_ch = price7, p_tf = price8, 
                                           p_fg = price9, p_dm = price10)] %>%
    .[, lapply(.SD, function(x) round(64 * x, 2)), by = week]
  
  
  prices64 <- as.matrix(prices64[,2:9])
  return(prices64)
}

#Truncated Poisson
rztpois2 = function(n, lambda, q = 1:30) {
  pztpoisson = lambda^q / (expm1(lambda) * gamma(q + 1))
  sample(q, n, prob = pztpoisson, replace = TRUE) 
}

#Function to Update Inventory
UpdateInventory <- function(inv, q, avg_cons){
  cons=min(inv, avg_cons)
  inv=inv+q-cons
  return(inv)
}

#Function to Sample Incidence
SampleIncidence <- function(gamma0, gamma2, gamma3, inv, avg_cons)
{
  util= gamma0+gamma2*avg_cons+gamma3*inv
  prob = 1/(1+exp(-util))
  incidence=rbinom(1,1,prob=prob)
  return(incidence)
}

#Function to Sample Quantity
SampleQuantity <- function(phi0)
{
  q=rztpois2(1, phi0)
  return(q)
}

#Function to Sample Brand Choice
SampleBrandChoice <- function(beta0,beta1,price)
{
  utility= beta0+beta1*price
  prob=exp(utility)/sum(exp(utility))
  b=which(rmultinom(1,1,prob=prob)==1)
  return(b)  #Returning the position
}


PromotionEffects <- function(H,
                             inv0, q0, avg_cons,
                             gamma0, gamma2, gamma3,
                             beta0, beta1, phi0){
price<-PriceMatrix()
W=nrow(price)
data_ht=NULL
pos=1
for(h in 1:H) {
  inv = inv0
  q = q0
  for(w in 1:W) {
    inv=UpdateInventory(inv, q, avg_cons)
    i=SampleIncidence(gamma0, gamma2, gamma3, inv, avg_cons)
    if(i==1){
      b=SampleBrandChoice(beta0, beta1, price[w,])
      q=SampleQuantity(phi0)
      data_ht[[pos]]=data.table(h=h, w=w, b=b,q=q)
    }
    if(i==0)
    {
      q=0
    }
    pos=pos+1
  }
}
datamain=rbindlist(data_ht)
return(datamain)
}

set.seed(40)
data <- PromotionEffects(H=3, 
                         inv0 = 2, q0 = 0, avg_cons = 0.5,
                         gamma0=0, gamma2=0.1, gamma3 = -0.1,
                         beta0=rnorm(8, mean=2, sd=0.3),
                         beta1=-1,
                         phi0=2)


