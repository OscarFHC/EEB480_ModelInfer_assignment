# When
# who

library(RCurl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lmtest)

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
  filter(ratio!="NA")
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
  
  
thrip.lm.gomp=
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

X = model.matrix(parus.lm.ricker) # design matrix
B = solve(crossprod(X), crossprod(X, parus.ex3$ratio))
  # I can also calculate the Beta matrix by myself.

vcovB = vcov(parus.lm.ricker) # variance-covariance matrix of the linear model (Ricker model)
  # The variance is the square of the standard error as there is only one beta (sample size=1)
  # I'm not entirely sure how the covariance is calculated...

fun2 = function(B){ # transformation function from coefficients to theta (carrying capacity in this case)
  theta2 = -(B[1]/B[2])
}

install.packages("numDeriv") # Here I installed a package to calculate the Jacobian matrix evaluated at the beta = beta hat, although I can calculate it by hand
library(numDeriv)

K.se = sqrt(jacobian(fun2, B) %*% vcovB %*% t(jacobian(fun2, B)))
K.se

# The following package allows me to calculate the standard error of a parameter that is being transformed from the coefficients estimated in a model.
#install.packages("msm")
library(msm)
deltamethod(~ -(x1/x2), coef(parus.lm.ricker), vcov(parus.lm.ricker))
### EX 8
### EX 9
parus.lm.ricker=lm(ratio~pop.nat, data=parus.ex3)
parus.ricker.AIC = AIC(parus.lm.ricker)

parus.lm.gomp=lm(ratio~pop.log, data=parus.ex3)
parus.gomp.AIC = AIC(parus.lm.gomp)

prob = exp((parus.gomp.AIC - parus.ricker.AIC)/2)
 # According to the AIC difference of the two models, the Ricker model, when comparing to the Gompertz model, is `r prob` times less likely to explain same amount of variance of the data. 
 # From the adjust R^2, 
### EX 9
### EX 10
thrip.ex3 = thrip  %>%
  mutate(pop.log=log(pop.nat)) %>%
  mutate(ratio=c(log(thrip[,"pop.nat"][2:nrow(thrip)]/thrip[,"pop.nat"][1:(nrow(thrip)-1)]), "NA")) %>%
  filter(ratio!="NA")
thrip.ex3[,"ratio"] = as.numeric(thrip.ex3[,"ratio"])

thrip.lm.ricker=lm(ratio~pop.nat, data=thrip.ex3)
thrip.ricker.AIC = AIC(thrip.lm.ricker)

thrip.lm.gomp=lm(ratio~pop.log, data=thrip.ex3)
thrip.gomp.AIC = AIC(thrip.lm.gomp)

prob = exp((thrip.gomp.AIC - thrip.ricker.AIC)/2)

acf(thrip.ex3$pop.log)
acf(thrip.ex3$ratio)

acf(ts(residuals(arima(thrip.ex3$ratio, c(4, 0, 1)))))

thrip.ex10 = thrip.ex3 %>%
  mutate(ratio.dese4 = ts(residuals(arima(thrip.ex3$ratio, c(4, 0, 1)))))
  
adjR = c()
adjR.1 = c()
mod = lm(ratio~pop.log + month, data=thrip.ex10)
adjR = summary(mod)$adj.r.square
mod.1 = lm(ratio.dese4~pop.log + month, data=thrip.ex10)
adjR.1 = summary(mod.1)$adj.r.square

for (i in seq(1:11)) {
  mod = lm(ratio~pop.log + poly(month,i), data=thrip.ex10)
  mod.1 = lm(ratio.dese4~pop.log + poly(month,i), data=thrip.ex10)
  adjR = c(adjR, summary(mod)$adj.r.square)
  adjR.1 = c(adjR.1, summary(mod.1)$adj.r.square)
}

adjR.lag = c()
adjR.lag.1 = c()
mod.lag0 = lm(ratio~pop.log + month, data=thrip.ex10)
adjR.lag = summary(mod.lag0)$adj.r.square
mod.lag0.1 = lm(ratio.dese4~pop.log + month, data=thrip.ex10)
adjR.lag.1 = summary(mod.lag0.1)$adj.r.square


mon = ts(thrip.ex3$month)
for (i in seq(1:11)){
  mod.lag = lm(ratio~pop.log + month + lag(mon, i), data=thrip.ex10)
  adjR.lag = c(adjR.lag, summary(mod.lag)$adj.r.square)
  mod.lag.1 = lm(ratio.dese4~pop.log + month + lag(mon, i), data=thrip.ex10)
  adjR.lag.1 = c(adjR.lag.1, summary(mod.lag.1)$adj.r.square)
}

adjR.all = as.data.frame(cbind(adjR, adjR.1, adjR.lag, adjR.lag.1)) %>%
  gather(key=AdjR2, value=val, adjR, adjR.1, adjR.lag, adjR.lag.1) %>%
  mutate(num = rep(seq(1:12), 4))

p1 = ggplot(adjR.all[which(adjR.all[,"AdjR2"]=="adjR"),], aes(x=num, y=val))+
  geom_point()+
  xlim(1, 12)+
  ylim(0.2, 0.8)+
  labs(title="no time lag", x="number of polynomial terms", y="adjust R2")
p2 = ggplot(adjR.all[which(adjR.all[,"AdjR2"]=="adjR.1"),], aes(x=num, y=val))+
  geom_point()+
  xlim(1, 12)+
  ylim(0.2, 0.8)+
  labs(title="4 month time lag in growth rate", x="number of polynomial terms", y="adjust R2")
#p12 = grid.arrange(p1, p2, ncol=2)

p3 = ggplot(adjR.all[which(adjR.all[,"AdjR2"]=="adjR.lag"),], aes(x=num, y=val))+
  geom_point()+
  xlim(1, 12.5)+
  ylim(0.2, 0.8)+
  labs(title="no time lag", x="number of time lag in month", y="adjust R2")
p4 = ggplot(adjR.all[which(adjR.all[,"AdjR2"]=="adjR.lag.1"),], aes(x=num, y=val))+
  geom_point()+
  xlim(1, 12.5)+
  ylim(0.2, 0.8)+
  labs(title="4 month time lag in growth rate", x="number of time lag in month", y="adjust R2")
#p34 = grid.arrange(p3, p4, ncol=2)

pall = grid.arrange(p1, p2, p3, p4, ncol=2)
### EX 10
### EX 11
 #11.a
plot(parus.lm.gomp)
bptest(parus.lm.gomp)
 #11.b
shapiro.test(residuals(parus.lm.gomp))
 #11.c
    # 30, 39, 78
 #11.d
dwtest(parus.lm.gomp)
 #11.e
summary(parus.lm.gomp)
### EX 11
### EX 12
 # basically repeat EX11
### EX 12
### EX 13

set.seed(543)
train.id = sample(seq(1: nrow(thrip.ex3)), nrow(thrip.ex3)/2, replace=FALSE)
train.mod = lm(ratio~pop.log, data=thrip.ex3[train.id,])

y.pred = predict(train.mod, newdata=thrip.ex3[-train.id,])
plot(thrip.ex3[-train.id,"ratio"]~y.pred, xlab="predicted value", ylab="observed value")
summary(lm(y.pred~thrip.ex3[-train.id,"ratio"]))






