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

#Removing Categorical Columns
credit$Gender<-NULL
credit$Student<-NULL
credit$Married<-NULL
credit$Ethnicity<-NULL
View(credit)

#Finding Relation between the features
correlationMatrix <- cor(credit[,1:12])
print(correlationMatrix)

#Dividing Data into training and testing sets
train_rows <- sample(1:nrow(credit), .75*nrow(credit))
x.train<-cbind(credit[train_rows,1:6],credit[train_rows,8:12])
x.test <-cbind(credit[-train_rows,1:6],credit[-train_rows,8:12])
y.train <- credit$Balance[train_rows]
y.test <- credit$Balance[-train_rows]

#Ridge Regression Model
#As we know for Ridge Regression value of alpha is 0. Also we will use cross validation for finding optimal/best value of tuning parameter(lambda)
Ridge.fit <- cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", 
                        alpha=0, family="gaussian")

#Predicting Balance on testing dataset
Ridge.predicted <- 
  predict(Ridge.fit, s=Ridge.fit$lambda.1se, newx=as.matrix(x.test))

#Calculating R-squared value
rss_ridge <- sum((Ridge.predicted - y.test) ^ 2)
tss_ridge <- sum((y.test - mean(y.test)) ^ 2)
rsq_ridge <- 1 - rss_ridge/tss_ridge
rsq_ridge

#R-squared value for Rigde Regression Model is 0.9325118

#PCA Regression Model
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
tst=predict(pca,x.test)
tst=data.frame(tst,y.test)

#By Scree plot we can see than 1st 8 Principal Components accounts for more 93% variation in the Data.Therefore we will neglect PC9,PC10,PC11 as they have very small impact in the variation of Data.
fmla1=y.train~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8

#Fitting MOdel on the training dataset
pca_model=lm(fmla1,data=trg)
#pca_model=ranger(fmla1,trg,num.trees=50,respect.unordered.factors = "order")

#Prediction on the testing Data
pca_pred=predict(pca_model,newdata = tst)

#Calculating R-squared value
rss_pca <- sum((pca_pred- y.test) ^ 2)
tss_pca <- sum((y.test - mean(y.test)) ^ 2)
rsq_pca <- 1 - rss_pca/tss_pca
rsq_pca

#R-squared value for PCA Regression Model is 0.6401766

#After comparing R-squared values for both the Regression Models
#0.9325118 for Ridge Regression
#0.6401766 for PCA Regression
#Therefore for this Credit Dataset Ridge Regression Model gives better Results/Predictions as compared to PCA Regression Model



