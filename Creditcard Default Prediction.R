#Libraries
library(caret)
library(ROCR)
library(corrplot)
set.seed(5890)

# Load dataset
path="M:/Sachin My files/R/CSV files/Bank credit card logistic/BankCreditCard.CSV"
cc=read.csv(path, header=T)

#Drop unwanted column
cc$Customer.ID=NULL

#Dataypes
str(cc)

#converting factor variables
cc$Gender=as.factor(cc$Gender)
cc$Academic_Qualification=as.factor(cc$Academic_Qualification)
cc$Marital=as.factor(cc$Marital)
cc$Repayment_Status_Jan=as.factor(cc$Repayment_Status_Jan)
cc$Repayment_Status_Feb=as.factor(cc$Repayment_Status_Feb)
cc$Repayment_Status_March=as.factor(cc$Repayment_Status_March)
cc$Repayment_Status_April=as.factor(cc$Repayment_Status_April)
cc$Repayment_Status_May=as.factor(cc$Repayment_Status_May)
cc$Repayment_Status_June=as.factor(cc$Repayment_Status_June)
cc$Default_Payment=as.factor(cc$Default_Payment)

# Ratio of the Y variable to check whether data is balanced or not
table(cc$Default_Payment)
prop.table(table(cc$Default_Payment))

# check for nulls
checknull=function(x)
  return(any(is.na(x)))

col_name = colnames(cc) [apply(cc, 2,checknull)]
if(length(col_name) > 0) {
  print("NULLs present") 
  print(col_name)
}else
  print("No NULLs")

# check for blanks
checkblank=function(x)
  return(any(x==""))

col_name = colnames(cc) [apply(cc, 2, checkblank)]
if(length(col_name) > 0){
  print("Blanks present")
  print(col_name)
}else 
  print("No Blanks")

# check for zero's
checkzero=function(x)
  return(any(x==0))

col_name = colnames(numdata) [apply(numdata, 2, checkzero)]
if(length(col_name) > 0){
  print("Zeros present")
  print(col_name)
}else 
  print("No Zeros")


#separate the data

#Numeric data
numcol=colnames(cc)[sapply(cc,is.numeric)]
numdata=cc[,numcol]
dim(numdata)

#Factorial data
faccol=colnames(cc)[sapply(cc,is.factor)]
facdata=cc[,faccol]
dim(facdata)

#check for factor variables
for(e in faccol){
  print(paste("unique values for factor",e))
  print(unique(cc[e]))
  print("-------------")
}

# Fixing Marital feature which has one extra level 
table(cc$Marital)
cc$Marital=as.character(cc$Marital)
cc$Marital[cc$Marital==0]=2 
cc$Marital=as.factor(cc$Marital)


# Check for negative values
checknegative=function(x)
  return(any(x<0))
colnames(numdata)[apply(numdata,2,checknegative)]

#Assigning negative values to zero

cc$Jan_Bill_Amount[cc$Jan_Bill_Amount<0]=0
cc$Feb_Bill_Amount[cc$Feb_Bill_Amount<0]=0
cc$March_Bill_Amount[cc$March_Bill_Amount<0]=0
cc$April_Bill_Amount[cc$April_Bill_Amount<0]=0
cc$May_Bill_Amount[cc$May_Bill_Amount<0]=0
cc$June_Bill_Amount[cc$June_Bill_Amount<0]=0


# outliers
for (e in colnames(numdata))
  boxplot(numdata[e],main=e,horizontal = T)

# check for multicollinearity
cor=cor(numdata_log)
corrplot(cor,method='number',type='lower',number.cex = 0.5)


# check whether the levels are same in both train and test
# levels_in_train == levels_in_test --> OK
# levels_in_train > levels_in_test --> OK
# levels_in_train < levels_in_test --> NOT OK (ERROR)

colnames(facdata)

l_train=levels(factor(train$Repayment_Status_April))
l_test=levels(factor(test$Repayment_Status_April))
print(paste('train=',length(l_train),'test=',length(l_test)))
if (any(length(l_train)<length(l_test))){
    print("factors in train are less than test") 
}else
    print("factors in train are more than test")

#The levels are same in both test and train sets

#Building logistic model with all variables

# shuffling the data as data is in standard format
suff_cc=runif(nrow(cc))
cc=cc[order(suff_cc),]

#split the data 
totalrows=nrow(cc)
s=sample(seq(1:totalrows),0.7*totalrows)
train=cc[s,]
test=cc[-s,]
print(paste('train=',nrow(train),'test=',nrow(test)))


# Build the model
m1=glm(Default_Payment~.,data =train,binomial(link = logit))
summary(m1)

#predictions

p1=predict(m1,test,type='response')
head(p1)
table(test$Default_Payment)
cutoff=0.5
c1=length(p1[p1<=0.5])
c2=length(p1[p1>0.5])
print(c1)
print(c2)

pred1=ifelse(p1<=0.5,0,1)
table(pred1)
head(pred1)
confusionMatrix(test$Default_Payment,factor(pred1),positive = "1")

table(test$Default_Payment)

#Model 2 after removing all highly correlated variables

# Removing columns which are highly correlated

cc$Jan_Bill_Amount=NULL
cc$Feb_Bill_Amount=NULL
cc$March_Bill_Amount=NULL
cc$April_Bill_Amount=NULL
cc$May_Bill_Amount=NULL


# Build the model
m2=glm(Default_Payment~.,data =train,binomial(link = logit))
summary(m2)

#predictions
p2=predict(m2,test,type='response')
head(p2)
table(test$Default_Payment)
cutoff=0.5
c1=length(p2[p2<=0.5])
c2=length(p2[p2>0.5])
print(c1)
print(c2)

pred2=ifelse(p2<=0.5,0,1)
table(pred1)
head(pred1)
confusionMatrix(test$Default_Payment,factor(pred2),positive = "1")

#After comparing model 1 and 2,model 1 with all variables is better then model 2 .

#model 3
#removing nonsiginificant features after model 2

cc$Jan_Bill_Amount=NULL
cc$Feb_Bill_Amount=NULL
cc$March_Bill_Amount=NULL
cc$April_Bill_Amount=NULL
cc$May_Bill_Amount=NULL

cc$Repayment_Status_May=NULL
cc$June_Bill_Amount=NULL
cc$Previous_Payment_March=NULL
cc$Previous_Payment_May=NULL
cc$Previous_Payment_June=NULL

#split the data 
totalrows=nrow(cc)
s=sample(seq(1:totalrows),0.7*totalrows)
train=cc[s,]
test=cc[-s,]
print(paste('train=',nrow(train),'test=',nrow(test)))


# Build the model
m3=glm(Default_Payment~.,data =train,binomial(link = logit))
summary(m3)

#predictions
p3=predict(m3,test,type='response')
table(test$Default_Payment)
cutoff=0.5
c1=length(p3[p3<=0.5])
c2=length(p3[p3>0.5])
print(c1)
print(c2)

pred3=ifelse(p3<=0.5,0,1)
confusionMatrix(test$Default_Payment,factor(pred3),positive = "1")

#Model4: Standarizing the model
# standaring the numeric data using minmax function.

num=colnames(cc1)[sapply(cc1,is.numeric)]
numdata1=cc1[,num]
View(cc1)
minmax = function(x) return( (x-min(x))/(max(x)-min(x)) )
cc_scaled=as.data.frame(lapply(numdata1,minmax))
View(cc_scaled)
dim(cc_scaled)
View(numdata1)


# outliers
for (e in colnames(numdata1))
  boxplot(numdata1[e],main=e,horizontal = T)

# check for multicollinearity
cor=cor(cc_scaled_numdata)
corrplot(cor,method='number',type='lower',number.cex = 0.5)

#Removing highly correlated variables
cc_scaled$Jan_Bill_Amount=NULL

#split the data 
totrows=nrow(cc_scaled)
s=sample(seq(1:totrows),0.7*totrows)
train_scaled=cc_scaled[s,]
test_scaled=cc_scaled[-s,]
print(paste('train=',nrow(train_scaled),'test=',nrow(test_scaled)))


# Build the model
m4=glm(Default_Payment~.,data =train_scaled,binomial(link = logit))
summary(m4)

#predictions
p4=predict(m4,test_scaled,type='response')
table(test_scaled$Default_Payment)
cutoff=0.5
c1=length(p4[p4<=0.5])
c2=length(p4[p4>0.5])
print(c1)
print(c2)

pred4=ifelse(p4<=0.5,0,1)
confusionMatrix(test_scaled$Default_Payment,factor(pred4),positive = "1")



#Model 5: Over sampling
library(ROSE)

over_cc=ovun.sample(Default_Payment~.,data=cc, method = 'over',N=45000,seed = 1)$data
table(over_cc$Default_Payment)
tt=nrow(over_cc)
over_s=sample(seq(1:tt),0.7*tt)
over_train=over_cc[over_s,]
over_test=over_cc[-over_s,]
print(paste('over_train=',nrow(over_train),'over_test=',nrow(over_test)))

m6=glm(Default_Payment~.,data=over_train,binomial(link = logit))
summary(m6)

# predictions
p6=predict(m6,over_test,type = 'response')
head(p6)
cutoff=0.4
length(p6[p6<=0.4])
length(p6[p6>0.4])

pred6=ifelse(p<=0.4,0,1)
head(pred6)

confusionMatrix(over_test$Default_Payment,factor(pred6),positive = '1')
table(over_test$Default_Payment)

# Model 6: SMOTE 

library(DMwR)
balanced_data=SMOTE(Default_Payment~.,cc,perc.over = 200,k=5,perc.under = 200)

table(balanced_data$Default_Payment)

tt=nrow(balanced_data);tt
sbalanced=sample(seq(1:tt),0.70*tt)
balancedtrain=balanced_data[sbalanced,]
balancedtest=balanced_data[-sbalanced,]
nrow(balancedtrain);nrow(balancedtest)

m7=glm(Default_Payment~.,data=balancedtrain,binomial(link = logit))
summary(m7)

#predictions
p7=predict(m7,balancedtest,type='response')
head(m7)
table(balancedtest$Default_Payment)
cutoff=0.4
c1=length(p7[p7<=0.4])
c2=length(p7[p7>0.4])
print(c1)
print(c2)

pred7=ifelse(p7<=0.4,0,1)
head(pred7)
confusionMatrix(balancedtest$Default_Payment,factor(pred7),positive = "1")
table(balancedtest$Default_Payment)

pp=prediction(m7,balancedtest$Default_Payment)

#After building models SMOTE model with high accuracy,senstivity,specificty and kappa value is selected

# identifying the best cut-off by plotting the ROC curve
# 1) evaluations
# ------------------------------------
evals = performance(pp, "acc")
evals
plot(evals)
abline(h=0.75, v=0.60) 
max_yval = which.max(slot(evals, "y.values")[[1]])
max_acc = slot(evals, "y.values")[[1]][max_yval]
max_cutoff = slot(evals, "x.values")[[1]][max_yval]
print(paste("Best accuracy = ", round(max_acc,4), 
  " Best Cutoff = ", round(max_cutoff,4)))

predictions_max = ifelse(p_smodel <= max_cutoff, 0,1)
tab = table(predicted = predictions_max,actual = balancedtest$Default_Payment)
confusionMatrix(as.factor(predictions_max),
  balancedtest$Default_Payment, positive="1")

# plot ROC
perf = performance(pp, "tpr", "fpr")
plot(perf, colorize=T, main="ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0, b=1)

# area under the curve (AUC)
auc = performance(pp, "auc")
round(unlist(slot(auc, "y.values")),3)
