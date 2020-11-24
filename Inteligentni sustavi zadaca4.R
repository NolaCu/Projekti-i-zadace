library(ISLR)
library(glmnet)
library(readr)
rm(list = ls()) #proèišæavanje workspace-a

#uèitavanje baze, kreiranje matrice x
student = read.csv(url("http://www.odraz.com/student-por.csv"), stringsAsFactors=TRUE, sep=";") 
attach(student)
y=G3
x=model.matrix(G3~., student)[,-1]
head(x)

#ridge regresija
lambda=10^seq(10,-2)
modelridge = glmnet(x, y, alpha = 0, lambda = lambda)
dim(coef(modelridge))
plot(modelridge)

#podjela skupa 
set.seed(123)
treniranje = sample(1:nrow(x), nrow(x)/2)
testiranje = (-treniranje)
testiranje
y_testiranje = y[testiranje]
y_testiranje

modelridge2=glmnet(x[treniranje,],y[treniranje],alpha=0,lambda=lambda, thresh=1e-12)
ridgepredikcija=predict(modelridge2,s=lambda,newx=x[testiranje,])
mse_pogreska<-mean((ridgepredikcija-y_testiranje)^2)
mse_pogreska

#unakrsna validacija
set.seed(123)
unakrsna = cv.glmnet(x[treniranje,], y[treniranje], alpha = 0)
plot(unakrsna)
najboljalambda = unakrsna$lambda.min
najboljalambda

#raèunanje MSE pogreÅ¡ke
predikcijaridge=predict(modelridge2, s=najboljalambda, newx=x[testiranje,])
mean((predikcijaridge-y_testiranje)^2)

#treniranje modela i koeficijenti
ponovo=glmnet(x,y,alpha=0)
predict(ponovo,type="coefficients",s=najboljalambda)[1:20,]

#lasso model
modellasso = glmnet(x[treniranje,], y[treniranje], alpha = 1, lambda = lambda)
plot(modellasso)
lassopredikcija=predict(modellasso,s=lambda,newx=x[testiranje,])
mean((lassopredikcija-y_testiranje)^2)
dim(coef(modellasso))

#unakrsna validacija za najbolju lambdu
set.seed(123)
unakrsna1=cv.glmnet(x[treniranje,],y[treniranje],alpha=1)
plot(unakrsna1)
bestlambda=unakrsna1$lambda.min
bestlambda
lassopredikcija2=predict(modellasso,s=bestlambda,newx=x[testiranje,])
mean((lassopredikcija2-y_testiranje)^2)

#treniranje na cijelom skupu i koeficijenti
ponovo2=glmnet(x,y,alpha=1)
predict(ponovo2,type="coefficients",s=bestlambda)

