# When
# who

library(RCurl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


dat1_raw=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/EEB480_ModelInfer_assignment/master/Mod_Infer_hw6/Parus_major_Wytham_Wood.csv"), 
                  sep=",", header=T,comment.char="#")
dat2_raw=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/EEB480_ModelInfer_assignment/master/Mod_Infer_hw6/Thrips_imaginis_Davidson1948.csv"), 
                  sep=",", header=T,comment.char="#")
parus = dat1_raw %>% 
  rename(yr=Sample.Date, pop.nat=Population)
thrip = dat2_raw %>%
  mutate(yr=Sample.Date%/%1,
         month=round(100*(Sample.Date%%1)),
         time=yr+month/12,
         pop.nat=Population,
         Population=NULL,
         Sample.Date=NULL)

### EX 1
parus.ex1 = parus %>%
  mutate(pop.sqrt=sqrt(pop.nat), pop.log=log(pop.nat)) %>%
  gather(key=variable, value=pop, -c(yr))
parus.ex1[,"variable"]=factor(parus.ex1[,"variable"], levels=c("pop.nat", "pop.sqrt", "pop.log"))

ggplot(data=parus.ex1, aes(x=yr, y=pop), group=variable)+
  geom_line()+
  facet_grid(variable ~ ., scales = "free_y")+
  xlab("year")+
  ylab("per capita population size")
  

thrip.ex1 = thrip %>%
  mutate(pop.sqrt=sqrt(pop.nat), pop.log=log(pop.nat)) %>%
  gather(key=variable, value=pop, -c(yr, month, time))
thrip.ex1[,"variable"]=factor(thrip.ex1[,"variable"], levels=c("pop.nat", "pop.sqrt", "pop.log"))

ggplot(data=thrip.ex1, aes(x=time, y=pop), group=variable)+
  geom_line()+
  facet_grid(variable ~ ., scales = "free_y")+
  xlab("year")+
  ylab("per capita population size")
  
  # ?? how to organize the format of date?

hist(parus[,"pop.nat"], breaks=15)
hist(thrip[,"pop.nat"], breaks=15)
### EX 1
### EX 2
parus.ex2 = parus  %>%
  mutate(pop.log=log(pop.nat)) %>%
  mutate(ratio=c(log(parus[,"pop.nat"][2:nrow(parus)]/parus[,"pop.nat"][1:(nrow(parus)-1)]), "NA")) %>%
  filter(ratio!="NA") %>%
  gather(key=variable, value=pop, -c(yr, ratio))
  
ggplot(data=parus.ex2, aes(x=pop, y=as.numeric(ratio)), group=variable)+
  geom_point()+
  facet_wrap(~variable, nrow=2, scales="free")+
  #facet_grid(variable~., scales = "free")+ #this is not working
  xlab("population size")+
  ylab("per capita population growth rate")

thrip.ex2 = thrip  %>%
  mutate(pop.log=log(pop.nat)) %>%
  mutate(ratio=c(log(thrip[,"pop.nat"][2:nrow(thrip)]/thrip[,"pop.nat"][1:(nrow(thrip)-1)]), "NA")) %>%
  filter(ratio!="NA") %>%
  gather(key=variable, value=pop, -c(yr, month, time, ratio))

ggplot(data=thrip.ex2, aes(x=pop, y=as.numeric(ratio)), group=variable)+
  geom_point()+
  facet_wrap(~variable, nrow=2, scales="free")+
  #facet_grid(variable~., scales = "free")+ #this works
  #facet_grid(.~variable, scales = "free")+ #this is not working..
  xlab("population size")+
  ylab("per capita population growth rate")

  # ?? why "acet_grid(variable~., scales = "free")" will not have free x and y scales?
### EX 2
### EX 3 and 4
parus.ex3 = parus  %>%
  mutate(pop.log=log(pop.nat)) %>%
  mutate(ratio=c(log(parus[,"pop.nat"][2:nrow(parus)]/parus[,"pop.nat"][1:(nrow(parus)-1)]), "NA")) %>%
  filter(ratio!="NA") %>%
parus.ex3[,"ratio"] = as.numeric(parus.ex3[,"ratio"])
  # Not sure if there is a better way to make the ratio variable into a numeric vector

parus.lm.ricker=lm(ratio~pop.nat, data=parus.ex3)
  summary(parus.lm.ricker)
parus.r.ricker=parus.lm.ricker$coefficients[1]
parus.K.ricker=-(parus.lm.ricker$coefficients[1]/parus.lm.ricker$coefficients[2])

parus.lm.gomp=lm(ratio~pop.log, data=parus.ex3)
  summary(parus.lm.gomp)
parus.r.gomp=-log(-parus.lm.gomp$coefficients[2])
parus.K.gomp=exp(parus.lm.gomp$coefficients[1]/-(parus.lm.gomp$coefficients[2]))


thrip.ex3 = thrip  %>%
  mutate(pop.log=log(pop.nat)) %>%
  mutate(ratio=c(log(thrip[,"pop.nat"][2:nrow(thrip)]/thrip[,"pop.nat"][1:(nrow(thrip)-1)]), "NA")) %>%
  filter(ratio!="NA")
thrip.ex3[,"ratio"] = as.numeric(thrip.ex3[,"ratio"])

thrip.lm.ricker=lm(ratio~pop.nat, data=thrip.ex3)
  summary(thrip.lm.ricker)
thrip.r.ricker=thrip.lm.ricker$coefficients[1]
thrip.K.ricker=-(thrip.lm.ricker$coefficients[1]/thrip.lm.ricker$coefficients[2])
  
  
thrip.lm.gomp=lm(ratio~pop.log, data=thrip.ex3)
  summary(thrip.lm.gomp)
thrip.r.gomp=-log(-thrip.lm.gomp$coefficients[2])
thrip.K.gomp=exp(thrip.lm.gomp$coefficients[1]/-(thrip.lm.gomp$coefficients[2]))
### EX 3 and 4  
### EX 5
parus.ex5 = parus.ex3  %>%
  gather(key=variable, value=pop, -c(yr, ratio)) %>%
  mutate(fit.ratio=c(predict(parus.lm.ricker), predict(parus.lm.gomp)))

ggplot(data=parus.ex5, group=variable)+
  geom_point(aes(x=pop, y=as.numeric(ratio)), col="black")+
  geom_line(aes(x=pop, y=as.numeric(fit.ratio)), col="blue")+
  facet_wrap(~variable, nrow=2, scales = "free")+
  xlab("population size")+
  ylab("population growth rate")

thrip.ex5 = thrip.ex3  %>%
  gather(key=variable, value=pop, -c(yr, month, time, ratio)) %>%
  mutate(fit.ratio=c(predict(thrip.lm.ricker), predict(thrip.lm.gomp)))

ggplot(data=thrip.ex5, group=variable)+
  geom_point(aes(x=pop, y=as.numeric(ratio)), col="black")+
  geom_line(aes(x=pop, y=as.numeric(fit.ratio)), col="blue")+
  facet_grid(.~variable, scales = "free")+
  xlab("population size")+
  ylab("population growth rate")
### EX 5
### EX 6
summary(parus.lm.ricker)
anova(parus.lm.ricker)
### EX 6
### EX 7
parus.ex7=parus.ex5 %>%
  filter(variable=="pop.nat")

confint(parus.lm.ricker, level=0.95)
confint(parus.lm.ricker, level=0.90)
confint(parus.lm.ricker, level=0.80)
confint(parus.lm.ricker, level=0.60)
ggplot(data=parus.ex7, aes(x=pop, y=as.numeric(ratio)))+
  geom_point(col="black")+
  geom_line(aes(x=pop, y=as.numeric(fit.ratio)), col="blue")+
  stat_smooth(method="lm", level=0.95)+
  stat_smooth(method="lm", level=0.90)+
  stat_smooth(method="lm", level=0.80)+
  stat_smooth(method="lm", level=0.60)
### EX 7
### EX 8
parus.lm.ricker=lm(ratio~pop.nat, data=parus.ex3)
summary(parus.lm.ricker)
parus.r.ricker=parus.lm.ricker$coefficients[1]
parus.K.ricker=-(parus.lm.ricker$coefficients[1]/parus.lm.ricker$coefficients[2])

X = model.matrix(parus.lm.ricker) # creat design matrix
B = solve(crossprod(X), crossprod(X, parus.ex3$ratio))
var(B)
cov(B)













