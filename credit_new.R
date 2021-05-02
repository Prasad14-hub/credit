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

#Remove rows containing NA values
#train<-na.omit()
#dim(credit)

#Remove/drop Unnessesary columns
credit$ID<-NULL
View(credit)

#Conversion of Categorical variables with N levels into N-1 indicater variables
fmla=Income~Limit+Rating+Cards+Age+Education+Gender+Student+Married+Ethnicity+Balance
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

correlationMatrix <- cor(credit[,1:12])
print(correlationMatrix)

#Expressing Dependent Variable in terms of Independent Variables
fmla=Income~Limit+Rating+Cards+Age+Education+Balance+Gender_Female+is_student+is_married+Ethnicity_Asian+Ethnicity_Caucasian

model=lm(fmla,credit)
#model=ranger(fmla,credit,num.trees=500,respect.unordered.factors = "order")
#model=gam(fmla,family=gaussian(),credit)

#credit$Predictions<-0
#splitplan<-kWayCrossValidation(nrow(credit),3,NULL,NULL)
#for(i in 1:3)
#{
#split<-splitplan[[i]]
#model=lm(fmla,data=credit[split$train,])
#credit$Predictions[split$app]=predict(model,newdata = credit[split$app,])
#}

train_rows <- sample(1:nrow(credit), .75*nrow(credit))
x.train <- credit[train_rows,2:12]
x.test <- credit[-train_rows,2:12]

y.train <- credit$Income[train_rows]
y.test <- credit$Income[-train_rows]

alpha0.fit <- cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", 
                        alpha=0, family="gaussian")

alpha0.predicted <- 
  predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=as.matrix(x.test))

mean((y.test - alpha0.predicted)^2)
rss <- sum((alpha0.predicted - y.test) ^ 2)
tss <- sum((y.test - mean(y.test)) ^ 2)
rsq <- 1 - rss/tss
rsq

pca_model<-prcomp(t(as.matrix(x.train)),scale=TRUE)
summary(pca_model)
predictions<-predit(fmla=)
#get_eigenvalue(pca_model)
#fviz_eig(pca_model,addlabels = TRUE)

#pca<-PCA(x.train, scale.unit = TRUE, ncp = 5, graph = TRUE)
#fviz_eig(pca,addlabels = TRUE)
#pca_model$x
#View(x.train)

## make a scree plot
pca.var <- pca_model$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca.data <- data.frame(Sample=rownames(pca_model$x),
                       X=pca_model$x[,1],
                       Y=pca_model$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")
