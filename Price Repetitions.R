#Script to create naive and basic replications of time series for future time period
library(bayesm)
library(data.table)
library(magrittr)
library(ggplot2)
library(data.table)

StrNum=5

PriceMatrix_Average <- function(StrNum, W){
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
  
  if(W > nrow(prices64))   #Average of last two periods
  {
    diff_weeks = W-nrow(prices64)
    prices64_2<-rbind(prices64, prices64[1:diff_weeks,])
    
    for(i in nrow(prices64):W){
      prices64_2[i,]<-round((prices64_2[i-1,]+prices64_2[i-2,])/2,2)
    }
    return(prices64_2)
  }
}



PriceMatrix_RepeatTimeSeries <- function(StrNum, W){
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
  
  if(W > nrow(prices64))   #Average of last two periods
  {
    diff_weeks = W-nrow(prices64)
    prices64_2<-rbind(prices64, prices64[1:diff_weeks,])
    return(prices64_2)
  }
}


melt(prices64, id.vars = "week") %>%
  ggplot(., aes(y = value, x = week, group = variable, color = variable)) +
  geom_line() + theme_minimal()

Average <- data.frame(PriceMatrix_Average(5, 230))
Repeat <- data.frame(PriceMatrix_RepeatTimeSeries(5, 230))

## Sample plot of Tropicana for Average vs Repeat 

library(ggplot2)

Tropicana <- data.frame(1:230, Average$p_trp, Repeat$p_trp)
colnames(Tropicana) <- c("Week","Average", "Repeat")

melt(Tropicana, id.vars = "Week") %>%
  ggplot(., aes(y = value, x = Week, group = variable, color = variable)) +
  geom_line() + theme_minimal()







