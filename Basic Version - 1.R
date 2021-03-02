### Basic Version of Promotion Effects - Neslin, van Heerde (2009) w/o any stockpiling other effects


library(data.table)
#Truncated Poisson
rztpois2 = function(n, lambda, q = 1:30) {
  pztpoisson = lambda^q / (expm1(lambda) * gamma(q + 1))
  sample(q, n, prob = pztpoisson, replace = TRUE) 
}

UpdateInventory <- function(inv, q, avg_cons){
  cons=min(inv, avg_cons)
  inv=inv+q-cons
  return(inv)
}

SampleIncidence <- function(gamma0, gamma2, gamma3, inv, avg_cons)
{
  util= gamma0+gamma2*avg_cons+gamma3*inv
  prob = 1/(1+exp(-util))
  incidence=rbinom(1,1,prob=prob)
  return(incidence)
}

SampleQuantity <- function(phi0)
{
  q=rztpois2(1, phi0)
  return(q)
}

SampleBrandChoice <- function(beta0,beta1,price)
{
  utility= beta0+beta1*price
  prob=exp(utility)/sum(exp(utility))
  b=which(rmultinom(1,1,prob=prob)==1)
  return(b)  #Returning the position
}

#This could go as a function
price <- as.matrix(prices64[,2:9])

set.seed(40)
H=3
W=nrow(price)
inv0 = 2 
q0 = 0
avg_cons = 0.5
gamma0  = 0.1
gamma2 = 0.1
gamma3 = -1
beta0=rnorm(8, mean=2, sd=0.3)
beta1=-1
phi0=2

# Add function for the prices from orange juice table as a function (basem - all weeks)
# give actual/relatively close beta0 values so.. MNL can be estimated. 
#function ( #add parameters + price ){close it, return datatable}


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

 
  



