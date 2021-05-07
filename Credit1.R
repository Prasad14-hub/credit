#Importing Required Libraries
library(tidyverse)
library(readxl)
library(tidyr)
library(tree)
library(ranger)
library(mgcv)
library(vtreat)
library(mlbench)
library(caret)
library(ISLR)
library(glmnet)
library(factoextra)
library(FactoMineR)
library(ggplot2)

set.seed(14)

credit<-Credit
View(credit)

#Data Info
head(credit)
summary(credit)
dim(credit)
str(credit)
names(credit)
glimpse(credit)
colSums(is.na(credit))


#As there is not any NA value present we dont need remove any row.

#Remove/drop Unnessesary columns
credit$ID<-NULL
View(credit)

#Conversion of Categorical variables with N levels into N-1 indicater variables
fmla=Balance~Limit+Rating+Cards+Age+Education+Gender+Student+Married+Ethnicity+Income
mmat=as.data.frame(model.matrix(fmla,credit))
View(mmat)


#Adding columns contaning indicator variables to Original Dataset
credit$Gender_Female<-mmat$GenderFemale
credit$is_student<-mmat$StudentYes
credit$is_married<-mmat$MarriedYes
credit$Ethnicity_Asian<-mmat$EthnicityAsian
credit$Ethnicity_Caucasian<-mmat$EthnicityCaucasian


#Removing Categorical columns after their conversion into the columns containing indicator variables
credit$Gender<-NULL
credit$Student<-NULL
credit$Married<-NULL
credit$Ethnicity<-NULL
View(credit)


#Finding Relation between the features/columns
correlationMatrix <- cor(credit[,1:12])
print(correlationMatrix)


#Selection of Variables for best model on the basis of R-squared,BIC,Cp,AIC values.
fix(credit)
library(leaps)
regfit.full=regsubsets(Balance~.,credit)
summary(regfit.full)
regfit.full=regsubsets(Balance~.,credit,nvmax = 11)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

#Plots between RSS-ncomp,R-squared-ncomp
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)
#7

points(7,reg.summary$adjr2[7],col="red",cex=2,pch=20)


#Plot between Cp and ncomp
plot(reg.summary$cp,xlab="Number of Variables ",ylab="Cp",
     type="l")
which.min(reg.summary$cp)
#6

points(6,reg.summary$cp[6], col ="red",cex=2,pch =20)
which.min(reg.summary$bic)
#4

#plot between BIC and ncomp
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type="l")
points(4,reg.summary$bic[4],col="red",cex=2,pch =20)


plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")

coef(regfit.full,4)

#Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(Balance∼.,data=credit, nvmax=11,
                       method ="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Balance∼.,data=credit, nvmax=11,
                         method ="backward")
summary(regfit.bwd)



#Dividing the Data into training and testing sets
train_rows <- sample(1:nrow(credit), .75*nrow(credit))
train<-credit[train_rows,]
test<-credit[-train_rows,]
x.train<-cbind(train[,1:6],train[,8:12])
x.test <-cbind(test[,1:6],test[,8:12])
y.train <- credit$Balance[train_rows]
y.test <- credit$Balance[-train_rows]

regfit.best=regsubsets(Balance∼.,data=train,
                        nvmax=11)

test.mat=model.matrix(Balance∼.,data=test)

val.errors =rep(NA ,11)
for(i in 1:11){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((test$Balance-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

#Making our own function for predictions
predict.regsubsets =function(object , newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata )
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
  }

regfit.best=regsubsets(Balance∼.,data=credit,nvmax=11)
coef(regfit.best,10)

#10 fold Cross Validation
k=10
folds=sample(1:k,nrow(credit),replace=TRUE)
cv.errors=matrix(NA,k,11,dimnames =list(NULL,paste(1:11)))

for(j in 1:k){
  best.fit=regsubsets(Balance∼.,data=credit[folds!=j,],
                         nvmax=11)
  for(i in 1:11){
    pred=predict(best.fit,credit[folds==j,],id=i)
    cv.errors[j,i]= mean((credit$Balance[folds==j]-pred)^2)
    }
  }

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b")
min(mean.cv.errors)

reg.best=regsubsets(Balance∼.,data=credit,nvmax=11)
coef(reg.best,6)


#After all the results we found that the when the number of components(ncomp) are 7 we get the best fitting Model
coef(reg.best,7)

#Making new dataframes containing the 7 best Variables
x.train.new=x.train[c("Income","Limit","Rating","Cards","Age","is_student","Gender_Female")]
x.test.new=x.test[c("Income","Limit","Rating","Cards","Age","is_student","Gender_Female")]





#--------------Ridge Regression Model--------------





#As we know for Ridge Regression value of alpha is 0. Also we will use cross validation for finding optimal/best value of tuning parameter(lambda)
Ridge.fit <- cv.glmnet(as.matrix(x.train.new), y.train, type.measure="mse", 
                       alpha=0, family="gaussian")

bestlam=Ridge.fit$lambda.min

#Predicting Balance on testing dataset
Ridge.predicted <- 
  predict(Ridge.fit,s=bestlam,newx=as.matrix(x.test.new))

#calculating MSE
mean((Ridge.predicted-y.test)^2)

#Calculating R-squared value
rss_ridge <- sum((Ridge.predicted - y.test)^2)
tss_ridge <- sum((y.test - mean(y.test))^2)
rsq_ridge <- 1 - rss_ridge/tss_ridge
rsq_ridge

#R-squared value for Ridge Regression Model is 0.9363641




#--------------PCA Regression Model--------------





pca<-prcomp(x.train,center=TRUE,scale=TRUE)
summary(pca)
attributes(pca)


eigenvalues=pca$sdev^2
eigenvectors=pca$rotation


# make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


#Graph for finding Percentage of Variance Each Principal Accounts for
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data


#Graph between 1st and 2nd PCs
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")


#Creating Dataframes containing PCs
trg=predict(pca,x.train)
trg=data.frame(trg,y.train)
tst=data.frame(predict(pca,x.test))


#By Scree plot we can see than 1st 7 Principal Components accounts for more 86% variation in the Data.Therefore we will neglect PC9,PC10,PC11 as they have very small impact in the variation of Data.
fmla1=y.train~PC1+PC2+PC3+PC4+PC5+PC6+PC7


#Fitting MOdel on the training dataset
pca_model=lm(fmla1,data=data.frame(trg))
#pca_model=ranger(fmla1,trg,num.trees=50,respect.unordered.factors = "order")


#Prediction on the testing Data
pca_pred=predict(pca_model,newdata = data.frame(tst))


#Calculating R-squared value
rss_pca <- sum((pca_pred- y.test) ^ 2)
tss_pca <- sum((y.test - mean(y.test)) ^ 2)
rsq_pca <- 1 - rss_pca/tss_pca
rsq_pca



#Predicting results using pcr() function in pls library.
library(pls)
library(pcr)

#Model fitting on entire data
pcr.fit=pcr(Balance∼., data= credit, scale=TRUE ,
            validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

#Model Fitting on training data
pcr.fit=pcr(Balance∼.,data=credit[train_rows,],scale=TRUE ,
            validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

#After viewing plot and summary of pcr.fit we found that lowest cross-validation error occurs when ncomp=7

pcr.pred=predict(pcr.fit,x.test,ncomp=7)

#Calculating MSE
mean((pcr.pred -y.test)^2)

#Calculating R-squared value
rss_pcr<-sum((pcr.pred-y.test)^2)
tss_pcr<-sum((y.test-mean(y.test))^2)
rsq_pcr<-1-rss_pcr/tss_pcr
rsq_pcr


#R-squared value for PCA Regression Model is 0.6467493


#After comparing R-squared values for both the Regression Models
#0.9325118 for Ridge Regression
#0.6401766 for PCA Regression
#Therefore for this Credit Dataset, Ridge Regression Model gives better Results/Predictions as compared to PCA Regression Model.


# Prepared by Prasad Gavhane
# 2nd yr Minerals and Metallurgical Engineering
# IIT(ISM) Dhanbad
# Under the Supervision of 
# Prof. Sayantee Jana
# IIM Nagpur

