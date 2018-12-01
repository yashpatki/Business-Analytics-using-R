#LOGISTIC REGRESSION
 
install.packages("caret")#for higher level regression run help on (train)
install.packages("lattice")
install.packages("ggplot2")
library(caret)
data(GermanCredit) #GermanCredit is in built in R which is invoked using data()
head(GermanCredit,10)
dim(GermanCredit)
#View(GermanCredit)
dd<-GermanCredit
write.csv(dd,"german.csv")
getwd()

str(GermanCredit)
summary(GermanCredit)

#converting class variable from bad/good to 1/0


GermanCredit$Class=ifelse(GermanCredit$Class=="Bad",1,0)
GermanCredit$Class[1:5]

# we will split our dataset into Train and Test data to reduce the error (flip of coin eg)
length(createDataPartition(GermanCredit$Class, p= 0.7, list = FALSE))
#inTrain<-createDataPartition(GermanCredit$Class, p= 0.7, list = FALSE)

set.seed(1234) #creates an instance of work environment
inTrain=createDataPartition(GermanCredit$Class, p= 0.7, list = FALSE)
Training=GermanCredit[inTrain,]

str(Training)
dim(Training)

Testing=GermanCredit[-inTrain,] #testing data must be mutually exclusive from training data
dim(Testing)

summary(Training$Class)
sum(Training$Class) #bad customers in training
sum(Testing$Class) #bad customers in testing

#Checking for Multicollinearity
library(usdm)
vifstep(GermanCredit[,-10], th=2)  #multicollinearity exists between x variables so we eliminate the y variable by writing [,Class]

# Logistic regression body
ModelVar<-glm(Y~X1+X2+X3,
              data = dd,
              family = binomial("logit")) #logit means log(y)

#for dummy linear regression
ModelVar<-glm(Y~X1+X2+X3,
              data = dd,
              family = gaussian(link = "identity") #identity means y*1


help("glm")

fit0=glm(Class~ Duration+InstallmentRatePercentage+ResidenceDuration+Age+NumberExistingCredits+
           NumberPeopleMaintenance+Telephone+ForeignWorker+CheckingAccountStatus.0.to.200+
           CheckingAccountStatus.gt.200+CheckingAccountStatus.none+CreditHistory.ThisBank.AllPaid+
           CreditHistory.Delay+CreditHistory.Critical+Purpose.UsedCar+Purpose.Furniture.Equipment+
           Purpose.Radio.Television+Purpose.DomesticAppliance+Purpose.Repairs+Purpose.Education+
           Purpose.Retraining+Purpose.Business+ Purpose.Other+SavingsAccountBonds.100.to.500+
           SavingsAccountBonds.500.to.1000+SavingsAccountBonds.gt.1000+SavingsAccountBonds.Unknown+
           EmploymentDuration.1.to.4+EmploymentDuration.4.to.7+
           EmploymentDuration.Unemployed+Personal.Female.NotSingle+Personal.Male.Married.Widowed+
           OtherDebtorsGuarantors.CoApplicant+OtherDebtorsGuarantors.Guarantor+Property.Insurance+
           Property.CarOther+OtherInstallmentPlans.Stores+OtherInstallmentPlans.None+
           Housing.Own+Job.UnskilledResident+Job.Management.SelfEmp.HighlyQualified,
         data=Training,family=binomial(link="logit"))

summary(fit0)


fit2d<-glm(Class~Duration + Age , data = Training , family = binomial(link = "logit"))
summary(fit2d)

#Select the model obtained from VIF for stepwise & run AIC to eliminate insignificant variables
install.packages("MASS")
library(MASS)
step=stepAIC(fit0, direction="both") #to get terms not eliminated by stepAIC use step$terms

opfitA= glm(Class~ Duration + InstallmentRatePercentage + Age + NumberExistingCredits + 
            Telephone + CheckingAccountStatus.gt.200 + CheckingAccountStatus.none + 
            CreditHistory.Critical + Purpose.UsedCar + Purpose.Furniture.Equipment + 
            Purpose.Radio.Television + Purpose.Retraining + Purpose.Business + 
            Purpose.Other + SavingsAccountBonds.100.to.500 + SavingsAccountBonds.500.to.1000 + 
            SavingsAccountBonds.gt.1000 + SavingsAccountBonds.Unknown + 
            EmploymentDuration.4.to.7 + Personal.Female.NotSingle + OtherDebtorsGuarantors.Guarantor + 
            Property.Insurance + Property.CarOther + OtherInstallmentPlans.None + 
            Housing.Own, data=Training,family=binomial(link="logit"))
summary(opfitA)

fitA= glm(Class~ Duration + InstallmentRatePercentage + Age + NumberExistingCredits + 
              Telephone + CheckingAccountStatus.gt.200 + CheckingAccountStatus.none + 
              CreditHistory.Critical + Purpose.UsedCar + Purpose.Furniture.Equipment + 
              Purpose.Radio.Television + Purpose.Retraining + Purpose.Business + 
              Purpose.Other + SavingsAccountBonds.100.to.500 + SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 + SavingsAccountBonds.Unknown + 
              EmploymentDuration.4.to.7 + Personal.Female.NotSingle + OtherDebtorsGuarantors.Guarantor + 
              Property.Insurance + Property.CarOther + OtherInstallmentPlans.None + 
              Housing.Own, data=Training,family=binomial(link="logit"))

summary(fitA)

fitfull= glm(Class~ Duration + InstallmentRatePercentage + Age + NumberExistingCredits + 
            Telephone + CheckingAccountStatus.gt.200 + CheckingAccountStatus.none + 
            CreditHistory.Critical + Purpose.UsedCar + Purpose.Furniture.Equipment + 
            Purpose.Radio.Television + Purpose.Retraining + Purpose.Business + 
            Purpose.Other + SavingsAccountBonds.100.to.500 + SavingsAccountBonds.500.to.1000 + 
            SavingsAccountBonds.gt.1000 + SavingsAccountBonds.Unknown + 
            EmploymentDuration.4.to.7 + Personal.Female.NotSingle + OtherDebtorsGuarantors.Guarantor + 
            Property.Insurance + Property.CarOther + OtherInstallmentPlans.None + 
            Housing.Own, data=GermanCredit,family=binomial(link="logit"))
summary(fitfull)

regTermTest(fitfull,"Personal.Female.NotSingle")

regTermTest(fitfull,"EmploymentDuration.4.to.7")

regTermTest(fitfull,"NumberExistingCredits")

regTermTest(fitfull,"Purpose.Business")


fitB=update(fitA, .~. -Property.Insurance-Pupose.Other-Purpose.Business-Purpose.Retraining-CreditHistory.ThisBank.AllPaid-Age-
              -SavingsAccountBonds.100.to.500-CheckingAccountStatus.gt.200-Purpose.Furniture.Equipment, data=Training)

summary(fitB)

fitC=update(fitB, .~. -Purpose.Other-SavingsAccountBonds.100.to.500-Property.CarOther-
              OtherInstallmentPlans.None , data=Training)
summary(fitC)

fitD=update(fitC, .~. -SavingsAccountBonds.500.to.1000 -Telephone -Housing.Own, data=Training)
summary(fitD)

#only if there are insignificant variables even at the end then do this or ignore
install.packages("survey",dependencies = TRUE)
library(survey)
regTermTest(fitD,"Personal.Female.NotSingle")

regTermTest(fitD,"EmploymentDuration.4.to.7")

regTermTest(fitD,"NumberExistingCredits")

#Stage 4 - Model Validation
#Checking Output on testing data

Pr1<-predict(fitD,Testing[,-10], type = "response") #removing column 10 because it is class
PredfitDodds<-predict(fitD,Testing[,-10],)

prclass<-ifelse(Pr1<0.5,0,1)

install.packages("InformationValue")
library(InformationValue)

decisionb=.5
finalpred<-ifelse(PredfitD<decisionb,0,1)

confusionMatrix(table(Testing$Class,finalpred,dnn = list('actual','predicted')))



Pred=predict(fitD,newdata=T)


install.packages("pscl",dependencies = TRUE)
library(pscl)
library(InformationValue)
plotROC(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))

ks_plot(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))
ks_stat(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD))