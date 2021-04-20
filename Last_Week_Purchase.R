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
