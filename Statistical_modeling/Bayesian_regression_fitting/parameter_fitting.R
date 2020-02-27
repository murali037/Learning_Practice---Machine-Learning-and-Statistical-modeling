o_mod = " model {

for(i in 1:length(logR)){

log.D_Sr[i] ~ dnorm(Mu[i], tau)

Mu[i] = (a+b/(Temp[i]+273.15))*logR[i]+c


}

a ~ dunif(-100, 100)
b ~ dunif(-100, 100)
c ~ dunif(-100, 100)

tau ~ dgamma(3, 1 / 112.8)

}
"



library(R2OpenBUGS)
library(coda)
library(rjags)
library(xlsx)
library(R2jags)


pm <- read.xlsx("parameter_fitting.xlsx",sheetIndex = 1)
pm <- pm[,!(names(pm)) %in% "NA."]
colnames(pm)[2] <- "Temp"


#Setup
parameters = c("a", "b", "c", "tau")

rdat = list(Temp = pm$Temp, logR = pm$log.R, log.D_Sr = pm$log.D_Sr.)

#Run it
set.seed(proc.time()[3])
rmod <- jags(model.file = textConnection(o_mod), parameters.to.save = parameters, 
             data = rdat, inits = NULL, 
             n.chains=9, n.iter = 10000, n.burnin = 1000, n.thin = 500)

#Results
rmod
rmod.mcmc = as.mcmc(rmod)
plot(rmod.mcmc)

# Simple Linear Regression foe estimating tau

D_Sr_fit <- lm(pm$log.D_Sr.~1+pm$Temp+pm$log.R, data = pm)

summary(D_Sr_fit)

#1/(0.05435)^2
#precision = 338.5

#shape = 3, scale = 112.8

# Inference for Bugs model at "5", fit using jags,
# 9 chains, each with 5e+05 iterations (first 10000 discarded), n.thin = 5000
# n.sims = 882 iterations saved
# mu.vect sd.vect     2.5%      25%      50%      75%   97.5%  Rhat n.eff
# a          -0.160   0.022   -0.197   -0.175   -0.162   -0.148  -0.112 1.005   610
# b          95.088   4.879   82.500   93.081   96.628   98.666  99.876 1.008   640
# c          -1.513   0.049   -1.605   -1.546   -1.513   -1.481  -1.419 1.001   880
# tau       240.004  53.279  144.339  203.034  238.416  270.633 363.065 1.003   720
# deviance -103.050   3.142 -107.190 -105.380 -103.616 -101.500 -95.223 1.009   620
# 
# For each parameter, n.eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor (at convergence, Rhat=1).
# 
# DIC info (using the rule, pD = var(deviance)/2)
# pD = 4.9 and DIC = -98.1
# DIC is an estimate of expected predictive error (lower deviance is better).

# Finding Estimated Value

D_Sr_est <- vector()
Mu <- vector()
a <- -0.160   #0.008                 # new estimates after changing the uniform dist lower limits from 0 to -100 
b <-  95.088  #2.705
c <-  -1.513  #0.024
pm <- mutate(pm, Log_D_Sr_Est = 0)


for(i in 1:length(pm$log.D_Sr.)){
  
  Mu[i] = (a+b/(pm$Temp[i]+273.15))*pm$log.R[i]+c
  pm$Log_D_Sr_Est[i] = Mu[i]
  
  print(Mu[i])
}

# Plotting it

png("../D_Sr_est.png", units="in", width=5, height=4.25, res=600)
plot(-10, 0, xlab = "Temp", ylab ="log_D_Sr",
     xlim=c(0,45), ylim=c(-1.4,-0.6),panel.first=grid())
axis(1)
axis(2)
box()

lines(pm$Temp,pm$log.D_Sr., col='red',type="o")
#lines(pm$Temp,pm$Log_D_Sr_Est, col='blue',type="o", lty = 2, pch = "+")

par(new = TRUE)
plot(pm$Temp,pm$Log_D_Sr_Est, type = "+", col='blue',axes = FALSE, 
     bty = "n",xlab = "", ylab ="log_D_Sr_est",xlim=c(0,45), ylim=c(-1.4,-0.6))
lines(pm$Temp,pm$Log_D_Sr_Est, col='blue',type="o", lty = 2, pch = "+")
axis(side=4, at = pretty(range(pm$Log_D_Sr_Est)))
mtext("log_D_Sr_est", side=4, line=3)

legend("topleft",legend=c("Log_D_Sr","Log_D_Sr_est"),
       text.col=c("red","blue"),pch=c("o","+"),col=c("red","blue"), cex = 0.5)


dev.off()

#scatter plot between log_D_Sr vs log_D_Sr_est

plot(pm$log.D_Sr., pm$Log_D_Sr_Est, main="Scatterplot log_D_Sr vs log_D_Sr_est", 
     xlab="log_D_Sr ", ylab="log_D_Sr_est ", pch=19)
abline(lm(pm$Log_D_Sr_Est~pm$log.D_Sr.), col="red") # regression line (y~x) 




