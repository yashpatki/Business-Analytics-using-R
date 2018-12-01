library(caret)
# loading the dataset
data(GermanCredit)
# check if the correct dataset is loaded
head(GermanCredit,10)
# Learn more about the dataset
dim(GermanCredit)
str(GermanCredit)

# identify the DV (Dependent Variable) 
summary(GermanCredit)

# Converting Class variable from Bad/Good to 1/0
GermanCredit$Class=ifelse(GermanCredit$Class=="Bad",1,0)

# Look at class to check DV if correctly coded or not
GermanCredit$Class[1:100]

# We will split our dataset into Train and Test data. The Train dataset will be used to build the model 
#and the Test dataset will used to validate how well our model is performing.
# However set seed prior to the creation of partition so that it not randonmised
set.seed(1234)
inTrain=createDataPartition(GermanCredit$Class, p=0.7, list=FALSE)
# Make training set from intrain rows
Training=GermanCredit[inTrain,]

# Make testing set from remaining intrain rows
Testing=GermanCredit[-inTrain,]

# Verify the dimensions of both train and testing
dim(Training)
dim(Testing)


sum(Training$Class) # Bad customers in training
sum(Testing$Class) # Bad customers in testing

# check for Multicollinearity, [,-10] represents the removal of Class fom data
library(usdm)
# Output of vifstep tell us how many are collinear variables, these have to be 
# removed from our analysis
vifstep(GermanCredit[,-10], th=2)

# Logistic regression Generalized format - this is not to be run!!!!
ModelVar<-glm(Y~X1+X2+X3,
              data = Data_set,
              family = binomial(link = "logit"))
# Linear regression Generalized format - this is not to be run!!! 
ModelVar<-glm(Y~X1+X2+X3,
              data = Data_set,
              family = gaussian(link ="identity"))

# Lets make our basic model after removal of collinear variables
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

# Look at the model
summary(fit0)

# Understanding the coefficients from a 2d model 
fit2d<-glm(Class~Duration+Age,data = Training,family = binomial(link="logit"))
summary(fit2d)

#Run step AIC to see which variables dont lead to reduction in AIC
library(MASS)
step=stepAIC(fit0, direction="both")
# final list from step
step

# MAke a model with StepAIC output
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

# Keep updating the model till you get rid of all insignificant variables

# Update 1
fitB=update(fitA, .~. -Property.Insurance-Pupose.Other-Purpose.Business-Purpose.Retraining-CreditHistory.ThisBank.AllPaid-Age-
              -SavingsAccountBonds.100.to.500-CheckingAccountStatus.gt.200-Purpose.Furniture.Equipment, data=Training)

summary(fitB)

# Update 2
fitC=update(fitB, .~. -Purpose.Other-SavingsAccountBonds.100.to.500-Property.CarOther-
              OtherInstallmentPlans.None , data=Training)
summary(fitC)

# Update 3
fitD=update(fitC, .~. -SavingsAccountBonds.500.to.1000 -Telephone -Housing.Own, data=Training)
summary(fitD)
# Note fitD will not have any insignificant variables

# Incase of any insignificant variable test the variable with Model 
library(survey)
regTermTest(fitD,"Personal.Female.NotSingle")
# Variable seems significant

# Now make prediction with your final model
Pred=predict(fitD, newdata=Training[,-10], type="response" )
Pred1=ifelse(Pred<0.5,0,1)
# Confusion Matrix of caret - not InformationValue!!!!!
confusionMatrix(table(Training$Class,Pred1,dnn=list('actual','predicted')))

# Lets start with Validation of Model
# Goodness of fit test
library(ResourceSelection)
hoslem.test(Training$Class,fitted(fitD),g=10)

# Mcfadden's R
library(pscl)
pR2(fitD)

# RoC curve - Note that we are using training data here.
# hence fitted function is used. In case you don't get this break the code down
# into respective functions so run fitted(fitD) first see the output.
# Also note we are passing probabilities because RoC will check for different
# threshold value we check for .5 and .4 thresholds in class!!
library(InformationValue)
plotROC(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))

# KS startistic - Look at the degree of separation between 0's and 1's
# This helps in validating model precision. 
ks_plot(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))
ks_stat(actuals=Training$Class,predictedScores=as.numeric(fitted(fitD)))
