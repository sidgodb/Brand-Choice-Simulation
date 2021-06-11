### Function for Since Last Week Purchase 

data_t <- data

Household1 <- subset(data, data$h==1)
Time_Since_Last_Purchase <- function(n)
#Manual Function
for(i in 1:nrow(data_t))
{
  if(i==1)
    {
    data_t$time[i]=data_t$w[i]
    }
  else{
  data_t$time[i]=data_t$w[i]-data_t$w[i-1]
  }
}
data_t$time <- ifelse(data_t$time<0, data_t$w, data_t$time)
data_t$time <- ifelse(data_t$w==1, 0, data_t$time)

#For complete dataframe
TF6 <- left_join(emdf, data, by=c("h","w"))
TF6$Purchase_Decision <- if_else(!(is.na(TF6$b)),1,0)

library(sqldf)

#Household1
Household1 <- subset(TF6, TF6$h==1)
Household1 <- TF6
#https://stackoverflow.com/questions/30391333/calculate-days-since-last-event-in-r
lastEvent <- NA
tae <- rep(NA, length(Household1$Purchase_Decision))
for (i in 2:length(Household1$Purchase_Decision)) {
  if (Household1$Purchase_Decision[i-1] == 1) {
    lastEvent <- Household1$w[i-1]
  }
  tae[i] <- Household1$w[i] - lastEvent
  
  # To set the first occuring event as 0 and not NA
  if (Household1$w[i]==1 && Household1$Purchase_Decision[i] == 1 && sum(Household1$Purchase_Decision[1:i-1] == 1) == 0) {
    tae[i] <- 0
  }
}

tae
TF8<-cbind(Household1, tae)
TF8$tae <- ifelse(TF8$tae<0|is.na(TF8$tae), 0, TF8$tae)
TF8

### Start incorporating Daniel's script here using datatable
library(data.table)
emdf <- CJ(h=1:H, w=1:W)
deff <- data_effects_4[, w_1 := shift(w), by=h]
deff2 = merge(emdf, deff, by = c("h", "w"), all.x = TRUE)
setkey(deff2, h, w)
deff2[, w_1 := nafill(w_1, type = "nocb"), by = h]
deff2[, weeks_since_purch := w - w_1]
deff2 = deff2[weeks_since_purch > 0]
deff2$PD <- ifelse(is.na(deff2$brandname),0,1)
model_pd <- glm(PD~weeks_since_purch, data=deff2, family="binomial")
summary(model_pd)
deff2[,.(Mean_TAE=mean(weeks_since_purch),Mean_PD=mean(PD)),by=h][,plot(Mean_TAE, Mean_PD)]
deff2[,mean(PD),by=h][,hist(V1)]

#### for Quantity and inventory
library(data.table)
emdf <- CJ(h=1:H, w=1:W)
data_effects_4 <- data
data_effects_4[,w_1 := shift(w), by=h]
data_effects_4[,wk_diff:= w-w_1]

#data_effects_4[,q_1 := shift(q), by=h]
#data_effects_4[,cons:=q_1/wk_diff]   #### total quantity divided by the week gap

deff2 = merge(emdf, data_effects_4, by = c("h", "w"), all.x = TRUE)
setkey(deff2, h, w)
deff2[, w_1 := nafill(w_1, type = "nocb"), by = h]
deff2[, weeks_since_purch := w - w_1]

deff3 = deff2[weeks_since_purch > 0]
deff3 = deff3[, inv:=NA]
deff4 = deff2[weeks_since_purch ==0]
deff4 = deff4[, inv:=2.5]

deff5= rbind(deff3, deff4)
deff5= deff5[order(h,w)]
deff5[, avg_cons:= 0.5, by=h]


lastpur <- deff5[,c(1,2,3,5,9,10,11)]
lastpur$cons_q <- lastpur$q-lastpur$avg_cons
lastpur[,cons_q:=nafill(cons_q, type="const", fill=-0.5), by=h]
lastpur1<-lastpur %>% group_by(h) %>% 
  mutate(inv = accumulate(cons_q, ~ .x + .y, .init = 2.5)[-1]) 
lastpur1$PD <- ifelse(is.na(lastpur1$b),0,1)
model_pd <- glm(PD~1+inv+weeks_since_purch, data=lastpur1, family="binomial")
summary(model_pd)

#deff2[, inv:=q, by=h]
#deff2[ ,inv:=nafill(inv, type="const", fill=0), by=h]

#Following not needed

#d_q <- data_effects_4[ ,q_1 := shift(q), by=h]
#data_effects_4[,acons:=0.5]
#deff[, inv:= q+q_1]
#deff2[, inv := nafill(inv, type="locf"), by=h]
#deff2[, avg_cons := nafill(avg_cons, type="nocb"), by=h]
#deff2[, q_1 := nafill(q_1, type = "locf"), by = h]
#deff2[, inv_new:= inv - shift(avg_cons), by=h]



### avg cons inventory ####
avgin <- deff2[,c(1,2,5,10,11)]
avgin <- data.frame(avgin)
avgin[,inv_new:=shift(inv)-q,by=h]


avg = subset(avgin, avgin$h==3)
matrix(avg1)<- avg1[,5:4]
avgin=data.table(avgin)
avgin[,inv_new := accumulate(inv,`-`)-avg_cons, by=h]

apply(avgin,1,function(d)f(d["inv"],d["avg_cons"]))

f <- function(inv, avg_cons)
{
  if(is.na(inv[i])){diff=0}
  else{
  diff[i]=inv[i-1]-avg_cons[i]
  inv[i]=diff[i]
  }
}

for(i in 4:nrow(avg))
{
  if(is.na(avg$inv[i-1])){diff=0}
  else{
    diff=max(avg$avg_cons[i],avg$inv[i-1]-avg$avg_cons[i])
    avg$inv[i]=diff
  }
}


### Repeated purchases
library(data.table)
library(dplyr)

RepeatPurchases<-function(H, W, data){
colnames(data) <- c("h","w","b","q","p")
emdf <- CJ(h=1:H, w=1:W)
data[,b_1 := shift(b), by=h]
data <- subset(data, !is.na(data$b_1))
data$RepeatPurchase <- ifelse(data$b==data$b_1, 1, 0)
RepeatInstances <- data %>% group_by(h) %>% summarize(RI = sum(RepeatPurchase),
                                                       TP = length(h))
RepeatInstances$Ratio<- (RepeatInstances$RI*100)/RepeatInstances$TP
return(mean(RepeatInstances$Ratio))
}

RepeatPurchases(100,100,data = data)

beta3 = seq(from = 0, to = 1, by = 0.5)
ratio <- c()
for(i in beta3)
{
  ratio=c(ratio,i)
}

