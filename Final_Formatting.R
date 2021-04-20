#Purchase Incidence Modeling 

install.packages("DescTools")
library(DescTools)
library(data.table)
library(dplyr)

### Analysis of Purchase Incidence Dependent on Other Factors & Last Purchase 
emdf <- CJ(h=1:H, w=1:W)
TF6 <- left_join(emdf, data, by=c("h","w"))
TF6$Purchase_Decision <- if_else(!(is.na(TF6$b)),1,0)

TF6<-TF8

BrandFreq <- data.table(table(TF6$brandname))

#Lag quantity, price and brand by one time unit
TF6$l_q <- lag(TF6$q, n=1)
TF6$l_price <- lag(TF6$price_per_unit, n=1)
TF6$l_b <- lag(TF6$brandname, n=1)

#Replace missing value with what's in previous time period?
TF6$q2 <- truncate_q(TF6$l_q)
TF6$b2 <- truncate_q(TF6$l_b)
TF6$p2 <- truncate_q(TF6$l_price)
TF6$time <- ifelse(is.na(TF6$time), mean(TF6$time,na.rm=T), TF6$time)
TF6$unit_sales <- TF6$q2*TF6$p2

#If one has to replace first week missing incidences with others 
FirstWeek = subset(TF6, TF6$w==1)
mean(FirstWeek$q, na.rm=T) # 2.11 
mean(FirstWeek$price_per_unit, na.rm=T) #1.99
Mode(FirstWeek$brandname, na.rm=T) #Tropicana

#Mean Quantity, brand, price for week 1 of every household
#First Purchase mean quantity is 1.5
TF6$q3 <- ifelse(TF6$w==1&is.na(TF6$q),2.11,
                 ifelse(is.na(TF6$q2),1.5, TF6$q2))

# Mean Quantity and Users Table for References 
MQ_U <- TF6 %>% group_by(h) %>% summarize(MQ = mean(q, na.rm=T))
MB_U <- TF6 %>% group_by(h) %>% summarize(MB = Mode(brandname, na.rm = T))


#Final logit model - #Inferences can be made based on log-odds /odd ratio comparison of brands
model <- glm(Purchase_Decision~-1+q2+b2, data=TF6, family="binomial") #AIC - 692544
model <- glm(Purchase_Decision~-1+unit_sales+b2, data=TF6, family="binomial") #AIC - 693003
summary(model)

model <- glm(Purchase_Decision~q2+brandname, data=TF6, family="binomial")
summary(model)

TF8[,.(.N,mean(Purchase_Decision)),keyby=tae]

#Subset data starting from the week AFTER the first purchase.
TF8_new <- subset(TF8, !is.na(TF8$tae)&TF8$tae>=0)
model <- glm(Purchase_Decision~tae*q2, data=TF8_new, family="binomial")
summary(model)


H1 <- subset(TF8_new, TF8_new$h==1)

TF8[,mean(Purchase_Decision),by=h][,hist(V1)]
TF8_new[,.(Mean_TAE=mean(tae),Mean_PD=mean(Purchase_Decision)),by=h][,plot(Mean_TAE, Mean_PD)]
TF8[,.(Mean_TAE=mean(tae),Mean_PD=mean(Purchase_Decision)),by=h][,plot(Mean_TAE, Mean_PD)]