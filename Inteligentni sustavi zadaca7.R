rm(list = ls())
library(ISLR)
library(gbm)

#attach dataseta
attach(Auto)
str(Auto)
set.seed(15)

#podjela indeksa 60 %-40%
indeks_treniranje = sample(1:nrow(Auto), round(0.6 * nrow(Auto)))

#stvaranje skupa za treniranje
Auto_treniranje = Auto [indeks_treniranje,]

#stvaranje skupa za testiranje
Auto_testiranje = Auto [-indeks_treniranje,]

#izrada boosting modela gbm
Auto_model1 = gbm(mpg ~ cylinders+weight+year+displacement+acceleration,
                 distribution = "gaussian", data = Auto_treniranje,
                 n.trees = 5000, interaction.depth = 4)

#graf relativne ovisnosti
summary(Auto_model1)

#graf relativne ovisnosti
par(mfrow=c(1,2))
plot(Auto_model1, i="weight")
plot(Auto_model1, i="displacement")

#predikcija prvog modela
predikcija1 = predict.gbm(object = Auto_model1, newdata = Auto_testiranje,
                          n.trees = 5000)
#izraèun toènosti tj.stope pogreške
mean((predikcija1-Auto_testiranje$mpg)^2)

#izrada poboljšanog modela
opt_auto <- gbm.perf(object = Auto_model1, 
                          method = "OOB", 
                          oobag.curve = TRUE)
opt_auto
Auto_model2 = gbm(mpg ~ cylinders+weight+year+displacement+acceleration,
                  distribution = "gaussian", data = Auto_treniranje,
                  n.trees = 102, interaction.depth = 4)
predikcija2 = predict.gbm(object = Auto_model2, newdata = Auto_testiranje,
                          n.trees = 102)
mean((predikcija2-Auto_testiranje$mpg)^2)



