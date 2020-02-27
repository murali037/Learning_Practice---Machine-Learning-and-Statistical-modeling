
#input data 
pm <- read.xlsx("parameter_fitting.xlsx",sheetIndex = 1)
pm <- pm[,!(names(pm)) %in% "NA."]
colnames(pm)[2] <- "Temp"

# Non-Linear Regression using LS fit using values for a,b and c as obtained above

#D_Sr_fit <- lm(pm$log.D_Sr.~1+pm$Temp+pm$log.R, data = pm)
D_Sr_fit <- nls(pm$log.D_Sr.~((a+(b/(pm$Temp+273.15)))*pm$log.R)+c, data = pm, start=c(a=-100,b=0,c=-100))

#get some estimation of goodness of fit
cor(pm$log.D_Sr.,predict(D_Sr_fit))  # 0.9516636

pm$predicted <- predict(D_Sr_fit)


#Summary
summary(D_Sr_fit)

# Formula: pm$log.D_Sr. ~ ((a + (b/(pm$Temp + 273.15))) * pm$log.R) + c
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#   a  -0.53262    0.05185  -10.27 2.20e-12 ***
#   b 214.17312   16.22852   13.20 1.45e-15 ***
#   c  -1.58690    0.03280  -48.37  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04268 on 37 degrees of freedom
# 
# Number of iterations to convergence: 2 
# Achieved convergence tolerance: 2.139e-08

###################################################### plots #####################################################



par(mfrow=c(2,1))
# plot temp vs D_Sr
plot(pm$Temp,pm$log.D_Sr., main="Scatterplot log_D_Sr_est vs Temp",xlab="Temp", ylab="log_D_Sr_est ", pch = 5)
lines(pm$Temp,predict(D_Sr_fit),lty=2,col="red",lwd=1)

#scatterplot using SNo as X axis
plot(pm$No.,pm$log.D_Sr., main="Scatterplot log_D_Sr_est vs S.No", 
     xlab="SNo", ylab="log_D_Sr_est ", pch=19)
lines(pm$No.,predict(D_Sr_fit),lty=2,col="red",lwd=1)





#scatterplot D_Sr vs D_Sr_est
par(mfrow=c(1,1))
plot(pm$log.D_Sr., predict(D_Sr_fit), main="Scatterplot log_D_Sr vs log_D_Sr_est", 
     xlab="log_D_Sr ", ylab="log_D_Sr_est ", pch=19)
lines(pm$log.D_Sr.,predict(D_Sr_fit),lty=2,col="red",lwd=1)


###################################################### plots #####################################################

#curve plot

plot(pm$log.D_Sr., predict(D_Sr_fit), main="Scatterplot log_D_Sr vs log_D_Sr_est", 
     xlab="log_D_Sr ", ylab="log_D_Sr_est ", pch=19)
p <- coef(D_Sr_fit)      # a= -0.5326207  b= 214.1731178  c= -1.5868965 
formula = function(x,y,a,b,c){ ((a+(b/(x+273.15)))*y)+c }
curve(formula(pm$log.D_Sr.,predict(D_Sr_fit),p["a"],p["b"],p["c"]), lwd=2, col="Red", add=TRUE)

