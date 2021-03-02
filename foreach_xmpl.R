library(foreach)
library(doParallel)
library(data.table)

cl = makeCluster(detectCores() - 2L)
registerDoParallel(cl)

tic = Sys.time()
res = foreach(i = seq.int(20),   #i=1:H 
              .combine = "rbind", 
              .multicombine = TRUE) %dopar% {
  Sys.sleep(1)
  c(i = i, x = rnorm(1))
}
toc = Sys.time()

stopCluster(cl)

toc - tic



cl = makeCluster(detectCores() - 2L)
registerDoParallel(cl)

tic = Sys.time()
res = foreach(i = seq.int(20), 
              .combine=function(...) rbindlist(list(...)), #enlist needed function
              .multicombine = TRUE, 
              .packages = "data.table") %dopar% {      #need to know packages
  Sys.sleep(1)
  data.table(i = i, x = rnorm(1))
}
toc = Sys.time()

stopCluster(cl)

toc - tic

