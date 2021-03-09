### Basic Version of Promotion Effects - Neslin, van Heerde (2009) w/o any stockpiling other effects

library(bayesm)
library(data.table)
library(magrittr)
library(ggplot2)
library(data.table)


#Function to create price matrix - from dataset orangejuice from bayesm package
PriceMatrix <- function(StrNum, W){
  data("orangeJuice")
  oj = data.table(orangeJuice$yx)
  orangeJuice<-0
  
  # we only use 64 oz package sizes and multiple price by 64 because raw prices are per oz.
  prices64 = oj[store == StrNum & brand == 1, .(week = 1:.N,
                                                p_trp = price1, p_fn = price3, 
                                                p_tr = price4, p_mm = price5, 
                                                p_ch = price7, p_tf = price8, 
                                                p_fg = price9, p_dm = price10)] %>%
    .[, lapply(.SD, function(x) round(64 * x, 2)), by = week]
  
  prices64 <- as.matrix(prices64[,2:9])
  
  if(W <= nrow(prices64))
  {
    return(prices64[1:W,])
  }
  
  if(W > nrow(prices64))   #Repeating Last Element [W - nrow(prices64)] times naive method
  {
    diff_weeks = W-nrow(prices64)
    prices64_2<-rbind(prices64,t(matrix(rep(prices64[nrow(prices64),],diff_weeks), nrow=8)))
    return(prices64_2)
  }
  
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


PromotionEffects <- function(seed, 
                             H,W,StrNum,
                             inv0, q0, avg_cons,
                             gamma0, gamma2, gamma3,
                             beta0, beta1, phi0){
  
  price<-PriceMatrix(StrNum, W)
  BrandNames <- c("Tropicana Premium","Florida's Natural","Tropicana",
                  "Minute Maid","Citrus Hill" ,"Tree Fresh","Florida Gold","Dominicks")
  W=nrow(price)
  data_ht=NULL
  pos=1
  set.seed(seed)
  for(h in 1:H) {
    inv = inv0
    q = q0
    for(w in 1:W) {
      inv=UpdateInventory(inv, q, avg_cons)
      i=SampleIncidence(gamma0, gamma2, gamma3, inv, avg_cons)
      if(i==1){
        b=SampleBrandChoice(beta0, beta1, price[w,])
        br = BrandNames[b]
        q=SampleQuantity(phi0)
        data_ht[[pos]]=data.table(h=h, w=w, b=b,brandname=as.character(br),
                                  inv=inv, avg_cons=avg_cons,
                                  q=q, price_per_unit=price[w,b])
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

#The following  will help you in deciding which Store Number to choose to be used in the function 
#below - for example: 2, 5, 8, 9, etc. 
unique(orangeJuice$yx$store)

data <- PromotionEffects(seed=40,H=10000, W=52, StrNum = 5,
                         inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                         gamma0=0, gamma2=0.1, gamma3 = -0.12,
                         beta0=c(0,-0.5,-0.72,-1.42,-1.59,-2,-2.1,-2.7),
                         beta1=-1,
                         phi0=0.25)

#setwd("/Users/Siddharth/Documents/")
write.csv(data, "SimulatedData.csv", row.names = F, sep = "")

#Time Required for different households
library(microbenchmark)

res<-microbenchmark(
  
  Ten = {
    PromotionEffects(seed=40,H=10, W=104, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  Hundred = {
    PromotionEffects(seed=40,H=100, W=104, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  Thousand = {
    PromotionEffects(seed=40,H=1000, W=104, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  TenThousands = {
    PromotionEffects(seed=40,H=10000, W=104, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  times = 5L ## specify the number of times each expression is evaluated
)

res_print<-data.frame(print(res, unit="s"))
#
#Unit: seconds
#expr         min          lq        mean     median          uq        max neval cld
#Ten   0.2904732   0.3332036   0.5310552   0.391117   0.4181394   1.222343     5  a 
#Hundred   1.0873858   1.1839010   2.6542583   2.086328   2.4667993   6.446878     5  a 
#Thousand  11.4317947  15.6530525  17.4512358  18.344201  19.9817932  21.845338     5  a 
#TenThousands 108.6167587 131.7115376 136.4220092 136.355202 151.3394893 154.087058     5   b

plotdata<-data.frame(x=c(10,100,1000,10000), y=c(0.53,2.65,17.45,136.42))

### Time Required for different Weeks - for a fixed household number - 1000

res_week<-microbenchmark(
  
  FiftyTwo = {
    PromotionEffects(seed=40,H=1000, W=52, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  HundredFour = {
    PromotionEffects(seed=40,H=1000, W=104, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  OneFifty = {
    PromotionEffects(seed=40,H=1000, W=150, StrNum = 5,
                     inv0 = 2.5, q0 = 0, avg_cons = 0.52,
                     gamma0=0, gamma2=0.1, gamma3 = -0.12,
                     beta0=c(0,-0.5,-0.5,-1.4,-1.59,-2,-2.1,-2.7),
                     beta1=-1,
                     phi0=0.25)
  },
  
  
  times = 5L ## specify the number of times each expression is evaluated
)

res_week_print<-data.frame(print(res_week, unit="s"))

#Unit: seconds
#expr       min        lq      mean    median        uq       max neval cld
#FiftyTwo  4.653853  4.792436  5.657088  4.933572  5.783911  8.121668     5 a  
#HundredFour  8.015238  8.753519  8.887058  8.762899  9.319437  9.584199     5  b 
#OneFifty 10.448927 11.284794 12.113065 11.745380 13.534967 13.551258     5   c


