#LINEAR REGRESSION IMPLEMENTATION

#Step 1: load the data
delivery=read.csv(file.choose(), header=TRUE)
head(delivery)

#step 2 :variable identification
str(delivery)
  
#step 3: Calculate the point estimates
summary(delivery)

#step 4: identification of Y and X variables
pairs(delivery)
cor(delivery)

#Step 5: check for Normality of Y
shapiro.test(delivery$DeliveryTime)

#not normal - shapiro value greater than 0.05
#not normal - shapiro output

delivery$transtime<-log(delivery$DeliveryTime)
head(delivery$transtime)

#shapiro test new variable
shapiro.test(delivery$transtime)

skewness(delivery$transtime)
install.packages("e1071",dependencies = TRUE)
library(e1071)
skewness(delivery$transtime) # skewness=0 for normal
kurtosis(delivery$transtime)

#check for autocorrelation - durbin watson
library("lmtest")
dwtest(delivery$transtime~delivery$Cases+delivery$Distance)



library(usdm)
vifstep(delivery[,-1], th=5) #not recommended, use vif() under library("car") or cor()
#multivariate model
fit1=lm(delivery$transtime~Distance + Cases,data = delivery)
summary(fit1)

#interaction model
fit2=lm(delivery$transtime~Distance:Cases, data = delivery)
summary(fit2)

#eliminate 1 variable

fit3=lm(delivery$transtime~Distance, data = delivery)
summary(fit3)

fit4=lm(delivery$transtime~Cases, data = delivery)
summary(fit4)

#with original data
fit5=lm(DeliveryTime~Distance, data = delivery)
summary(fit5)

#all models violate normality
anova(reduced,full)
anova(reduced,full)

fit
predict(fit1,data.frame(Cases=7,Distance=100))
predict(fit2,data.frame(Cases=7,Distance=100))
predict(fit3,data.frame(Cases=7,Distance=100))
predict(fit4,data.frame(Cases=7,Distance=100))
predict(fit5,data.frame(Cases=7,Distance=100))

#heteroskedascity test
#instal library lmtest

library(lmtest)
bptest(fit4) #to check heteroskedascity

#residual plot to crosscheck (proves no heteroskedascity visually)
plot(residuals(fit3)) 

#better look at our best model
summary(fit4) #check model parameters
coef(fit4) #impact of x variable
plot(fit4) #plot model
exp(fitted(fit4)) #fitted values

install.packages("car")
library(car)
outlierTest(fit4) #used to detect outliers. The p value is greater than 0.05 so there are no outliers


#incase of heteroskedascity use the following (rare)
library(sandwich)
vcovHC(fit3, Omega = NULL, type="Hc4")
coeftest(fit3,vcov =vcovHC(Model, Omega = NULL, type="HC4")) #eliminates heteroskedascity


#model validation
install.packages("ineq")
library(ineq)


ineq(fitted(fit3),type = "Gini") #gini co-eff gives the level of disparity
ineq(fitted(fit4),type = "Gini")
line<-fitted(fit4)
line2<-fitted(fit3)
par(mfrow= c(1,2))
plot(Lc(line),col = 'red',lwd=4)
plot(Lc(line2))

plot(Lc(line),col = 'red',lwd=4)
plot(Lc(fitted(fit4))) #lorenz curve (opposite side of gini)

print(paste("Gini Index:",gc*100," Gini Coefficient:",))
