### Script for dynamic inventory


#Truncated Poisson
rztpois2 = function(n, lambda, q = 1:30) {
  pztpoisson = lambda^q / (expm1(lambda) * gamma(q + 1))
  sample(q, n, prob = pztpoisson, replace = TRUE) 
}

DynamicInventory <- function(H,W, gamma0, gamma2, gamma3){
    datadyn=NULL
    pos=1
    for(h in 1:H){
      cons_h=rnorm(1, 10, sd=0.35)
          for(t in 0:W){
        if(t==0){
            inv=1   #initial value of inv 
            q=3     #initial value of quantity 
            util_in=0
          }
          if(t>0)
          {
            inv=inv+q-cons_h
            q=rztpois2(1,0.4)     #Need a better way to figure out how to model quantity
            util_in = gamma0+gamma2*cons_h+gamma3*inv
            util_in = round(util_in, 2)
          }
          
          #util_in = gamma0+gamma2*cons_h+gamma3*inv
          #util_in = round(util_in, 2)
          
          prob = 1/(1+exp(-util_in))
          prob = round(prob, 3)
          
          datadyn[[pos]]=data.table(h=h, t=t,cons_h=cons_h,inv=inv, q=q, 
                                    gamma0=gamma0, gamma2=gamma2,gamma3=gamma3,
                                    util_in=util_in,
                                    prob = prob)
          pos=pos+1
        }
      }
        datadyn=rbindlist(datadyn)
        return(datadyn)
}

dynamicInv <- DynamicInventory(5,10, 0,1,1)
