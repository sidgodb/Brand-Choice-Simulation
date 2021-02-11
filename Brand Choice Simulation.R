# This functions simulates a brand choice data based on the equations as highlighted
# by Neslin van Heerde (2009) in their promotion dynamics.
# Probablity outputs as as per equation A.7 (Probablity of household h, choosing brand b in week w)
# Utility is calculated as beta_0 + price*beta_1. (Eqn A8, Pg 252)
# Beta_1 = delta_0 + delta_1*previous values.. for current version, beta_1 = delta_1 (Eqn A10, Pg 252)


library(data.table)
library(dplyr)


BrandSimulation <- function(h,   #number of households
                            b,  #number of brands. 
                            w,   #number of weeks. 
                            pricemeans, pricedeviations,   #for everybrand, mean and deviation for prices
                            householdmeans, householddeviations, #for everybrand, mean and deviation for households (Beta_0)
                            delta0)
{
  set.seed(50)
  price<-c()
  for (i in 1:b)
  {
    price[[i]]<-rnorm(1:w, mean=pricemeans[i], sd=pricedeviations[i])
  }
  
  #Converting it in price-brand dataframe/unlist operation
  pricemat<-data.frame(t(sapply(price,c)))
  
  #joining brandnames to the dataframe
  brandprice<-data.frame(LETTERS[1:b],pricemat)
  colnames(brandprice) <- c("Brand",1:w)
  
  #Melt function to get Repeated Brands-Week combination
  brandprice<-melt(brandprice, id=1, measure=2:(w+1))
  colnames(brandprice)<-c("Brand","Weeks","Price")
  #return(brandprice)
  
  
  households=1:h
  set.seed(50)
  beta0<-c()
  for (i in 1:b)
  {
    beta0[[i]]<-rnorm(1:h, mean=householdmeans[i], sd=householddeviations[i])
  }
  
  #Converting it in brand-beta0 dataframe/unlist operation
  betamat<-data.frame(t(sapply(beta0,c)))
  
  #joining brand names to the data frame
  betahousehold<-data.frame(LETTERS[1:b],betamat)
  colnames(betahousehold) <- c("Brand",1:h)
  
  #Melt function to get Repeated Beta0s, Brands combination
  betahousehold<-melt(betahousehold, id=1, measure=2:(h+1))
  colnames(betahousehold)<-c("Brand","Households","Beta_0")
  
  
  #MergeDataFrames - Brands, Households, Week info.
  DF <- merge(brandprice, betahousehold, by="Brand")
  DF$Beta_1 <- delta0
  
  #Utility
  DF$Utility <- DF$Beta_0 + DF$Beta_1*DF$Price
  
  #ExpUtility
  DF$ExpUtility <- exp(DF$Utility)
  
  #Calculation of SumUtility
  sum <- DF %>% dplyr::group_by(Weeks, Households) %>% dplyr::summarize(TotalExpUtility=sum(ExpUtility))
  
  DF2<-base::merge(DF, sum, by=c("Weeks","Households"))
  
  DF2$Probablity<- DF2$ExpUtility/DF2$TotalExpUtility
  
  #Taking only Weeks, Households, Brand, Probabilities for randomization
  DF3 <- DF2[c(1,2,3,10)]
  DF3 <- dcast(DF3, Weeks+Households~Brand)
  
  #Creating a matrix for Weeks, Households, Choice
  DF4 <-matrix(ncol=3,nrow=h*w)
  
  #Defining the number of calls to traverse
  k=3+b-1
  
  for(i in 1:nrow(DF4))
  {
    DF4[i,1]<-DF3$Weeks[i]
    DF4[i,2]<-DF3$Households[i]
    DF4[i,3]<-LETTERS[which(t(rmultinom(1,1,DF3[i,3:k]))==1)]
  }
  
  DF4<-data.frame(DF4)
  colnames(DF4) <- c("Weeks","Households","Choice")
  
  #Combining the dataframes together
  Output<-merge(DF2,DF4, by=c("Weeks","Households"))
  
  Output$BinaryChoice<-ifelse(as.character(Output$Choice)==as.character(Output$Brand),"Yes","No")
  
  return(Output)
}

SimulatedOutput<-BrandSimulation(h=1000, #10 households
                                 b=3, #6 Brands
                                 w=104, #10 weeks 
                                 #Brand Price Means and Deviation are given in separate vectors 
                                 #as below
                                 #Brand A(mean=4,50, sd=0.23), Brand C(mean=1.89,sd=0.18)
                                 #Brand B(mean=6.40, sd=0.20), Brand D(mean=2.08,sd=0.09)
                                 pricemeans =c(4.50,4.40,4.40), #Price Means of Brands in sequential order
                                 pricedeviations =c(0.23,0.4,0.25), #Price SD of Brands in sequential order
                                 #Beta 0 for brands.BrandA norm(mean=0.25, sd=0.125) and so on. Similar to prices
                                 #Brand B Beta0 = norm(0,0). Brand B is reference brand.
                                 householdmeans = c(0.25,0,0.30),
                                 householddeviations = c(0,0,0),  #set it to zero for 
                                 delta0 = -1.78)

#Additional Comments: In this version, price series and household betas are normally distributed
#No Price Series graphs are plotted.
#Function just gives final table as output.
