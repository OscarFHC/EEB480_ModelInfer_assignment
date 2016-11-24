library(plyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(grid)
library(scales)
library(bbmle)

gt = read.csv("http://kinglab.eeb.lsa.umich.edu/480/data/gophertortoise.csv",
              comment.char="#",colClasses=c(date="Date",sex="factor",elisa="factor")) %>%
  arrange(carapace.length,date)
gt1 = gt %>% 
  ddply(~id,subset,date==min(date)) %>% 
  mutate(serostatus=ifelse(elisa=="neg",0,1))

# The above is just read in data.
# In the following I show how to do
# 1. code everything in single line

loglik = function (p, serostatus) {
  sum(log(dbinom(x=serostatus,size=1,prob=p)))
}

nll.2 = function (lam, carapace.length, serostatus) { 
  -loglik(p=1-exp(-(carapace.length*lam)), serostatus)
}

# The above is to calculate log likelihood in a function and then call the
# funciton in another function that is being optimized (finding the minimum)
 
 
nll.2.1 = function (lam, carapace.length, serostatus) { 
  -sum(log(dbinom(x=serostatus, size=1, prob=1-exp(-(carapace.length*lam)))))
}

# The above is to code everything in one single line.

fit.2 = mle2(nll.2, start=list(lam=0.005),data=gt1)
fit.2.1 = mle2(nll.2.1, start=list(lam=0.005),data=gt1)
