
library(mlogit)
library(gmnl)
library(nnet)
library(dplyr)

#### DATA VISUALIZATION FUNCTIONS #######

Demand_Function <- function(data)
{
  demand <- data %>% group_by(w, brandname) %>% summarise(Total_Sales=sum(q), Avg_Price=mean(price_per_unit))
  return(ggplot(data = demand, mapping = aes(y = log(Total_Sales), x = log(Avg_Price))) +
    geom_point(size=0.6) +
    facet_wrap(facets = vars(brandname)))
}

Purchase_Incidences <- function(data)
{
  purch <- data %>% group_by(w, brandname) %>% summarise(Total_Purchase_Incidents=length(h), Avg_Price= mean(price_per_unit))
  return(ggplot(data=purch, mapping=aes(y=Total_Purchase_Incidents, x=Avg_Price)) +
           geom_point(colour="dark blue", size=0.6)+
           facet_wrap(facets = vars(brandname)))
}

#### MNL ESTIMATION FOR UTILITY #########
DataPrepForMNLAnalysis <- function(data, StrNum, W){
  price_mat <- PriceMatrix(StrNum, W)
  colnames(price_mat) <- c("Tropicana Premium","Florida's Natural","Tropicana",
                           "Minute Maid","Citrus Hill" ,"Tree Fresh","Florida Gold","Dominicks")
  
  price_Mat2 <- reshape2::melt(price_mat)
  colnames(price_Mat2) <- c("w","brandname","price")
  data2 <- right_join(data, price_Mat2, by=c("w"))
  data2$q2 <- if_else(data2$brandname.x==data2$brandname.y,as.numeric(data2$q),NULL)
  data2$decision <- if_else(!is.na(data2$q2),"Yes","No")
  
  data3 <- data2[,c(1,2,7,8,9,10)]
  colnames(data3) <- c("h","w","brandname","price","q","decision")
  return(data.frame(data3))
}

MNLPrediction <- function(data)
{
  data.mnl <- mlogit.data(data.frame(data), choice="decision", shape="long", alt.var = "brandname")
  model <- mlogit(decision~price, data.mnl, seed=40, reflevel = "Tropicana Premium")
  return(model)
}


data.mnl <- DataPrepForMNLAnalysis(data, StrNum=5, W=50)
model.mnl <- MNLPrediction(data.mnl)
summary(model.mnl)

### Truncated Poisson Regression
library(VGAM)
#Ref:#https://stats.idre.ucla.edu/r/dae/zero-truncated-poisson/

#For one unit increase in price, the expected log odds of count will go down by -0.12
m3 <- vglm(q~price_per_unit, data=data_effects_3, family=pospoisson())
summary(m3)


#### Purchase Incidence Estimation ###########

#Function to calculate time since last purchase
TimeSinceLastPurchase <- function(data, H, W){
  library(data.table)
  emdf <- CJ(h=1:H, w=1:W)
  data_1 <- data[, w_1 := shift(w), by=h]
  data_2 = merge(emdf, data_1, by = c("h", "w"), all.x = TRUE)
  setkey(data_2, h, w)
  data_2[, w_1 := nafill(w_1, type = "nocb"), by = h]
  data_2[, weeks_since_purch := w - w_1]
  data_2 = data_2[weeks_since_purch > 0]
  data_2$PurchaseDecision <- ifelse(is.na(data_2$brandname),0,1)
  return(data_2)
}

#Purchase Incidence Regressed ONLY on Time Since Last Purchase
data_new <- TimeSinceLastPurchase(data_effects_4, 10000,100)
model_logit <- glm(PurchaseDecision ~ weeks_since_purch, data = data_new, family="binomial")
summary(model_logit)

#Purchase Incidence Regressed on Previous Quantity ? - TBD



