library(arules)
library(ggplot2)
library(caret)
library(tidyverse)
attach(podaci)
library(MLmetrics)
library(randomForest)
library(lattice)
library(plotly)
rm(list=ls())

#ureivanje i spajanje skupova podataka
str(data1)
str(data2)
data1 = data1[,-c(1,2,3,4,5,6,7,8,9,11,13,14,15,16,18, 19,23)]
str(data1)
data2 = data2[, -c(1,2,3,4,5,7,9,10,11,12,14,15)]
str(data2)
podaci=rbind(data1,data2)
str(podaci)

#zaokruûivanje podataka na decimale zbog razliËitih vrijednosti u skupovima
round(podaci, digits=2)

#ispitujemo korelaciju varijabli kako bi odabrali ispravne parametre za izradu analize
cor(podaci)

#1.zadatak - vizualizacija skupa podataka
ggplot(podaci, aes(x=volume.cm3, y=density.g.cm3))+geom_point(colour="orange")
ggplot(podaci, aes(x=Zs, y=FirmnessAv))+geom_point(color="orange")
ggplot(podaci, aes(x=Œ∏, y=FirmnessAv))+geom_point(colour="orange")

#histogram
hist(FirmnessAv, main = 'vrijednsoti varijable FirmnessAv',xlab = 'Zrelost breskve', col='orange')

#density plot
densityplot(~FirmnessAv, main="Density plot",  xlab="Zrelost breskve")

#boxplot(podaci[,0:6], main='Multiple Box plots')

plotic = plot_ly(data = podaci, x =~Zs, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
plotic = plot_ly(data = podaci, x =~Œ∏, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
plotic = plot_ly(data = podaci, x =~volume.cm3, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
summary(podaci)

#izrada modela
#podjela podataka na skupove
set.seed(35)
treniranje = sample(1:nrow(podaci), 0.8*nrow(podaci))
testiranje = podaci[-treniranje,"FirmnessAv"]

#podeöavanje parametara da se MSE smanji (u startu je bilo 0.7..)
#sluËajna öuma na temelju prve varijable - Zs
set.seed(35)
suma2=randomForest(FirmnessAv~Zs,data=podaci,subset=treniranje,importance=TRUE, 
                   ntree=994, sampsize=7, nPerm=8,# u ovom modelu dodavanjem parametra nPerm smanjuje MSE greöku
                   nodesize=1, maxnodes=8)#, do.trace=TRUE) #manji samp size - bolji rezultati

#radimo trace za praÊenje greöke po stablima 
#predikcija treÊeg modela 
predikcija3 = predict(suma2,newdata=podaci[-treniranje,])

#graf predikcije
plot(x=predikcija3, testiranje, main="Graf predikcije prvog modela sluƒçajne ≈°ume",
     xlab="predviƒëena vrijednost", ylab="stvarna vrijednost", col="orange")
abline(0,1)

#MSE pogreöka treÊeg modela
MSE(predikcija3, testiranje)

#sluËajna öuma na temelju druge varijable 
set.seed(35)
suma4=randomForest(FirmnessAv~Œ∏,data=podaci,subset=treniranje,importance=TRUE, 
                   ntree=1510, sampsize=50,
                   nodesize=1, maxnodes=8)#, do.trace=TRUE) #poveÊavanjem parametra nPerm poveÊavamo MSE greöku modela

#predikcija treÊeg modela 
predikcija4 = predict(suma4,newdata=podaci[-treniranje,])

#graf predikcije
plot(x=predikcija4, testiranje, main="Graf predikcije drugog modela sluƒçajne ≈°ume",
     xlab="predviƒëena vrijednost",ylab="stvarna vrijednost", col="orange")
abline(0,1)

#MSE pogreöka treÊeg modela
MSE(predikcija4, testiranje)

#plot slucajne sume
plot(suma2, col="orange")# error nad podacima za uƒçenje
plot(suma2$rsq, type="l", col="orange") #r2
plot(suma4, col="orange")# error nad podacima za uƒçenje
plot(suma4$rsq, type="l", col="orange") #r2


#2.zadatak
importance(suma2)
varImpPlot(suma2)

#iz ove funkcije vidimo da najveÊi znaËaj za predikciju zrelosti breskve imaju
#varijable Zs i Œ∏
filterVarImp(podaci[1:6], podaci$FirmnessAv, nonpara = TRUE)
importance(suma4)
varImpPlot(suma4)

#logistiËka regresija pomoÊu glm funkcije 

#diskretizacija ciljne varijable
podaci$FirmnessAv=discretize(FirmnessAv, method = "frequency", breaks = 2,labels =c("nezrela", "zrela"))
head(podaci)

#podjela podataka
train = sample(1:nrow(podaci), 0.8*nrow(podaci))
treniranje = podaci[train,]
testiranje = podaci[-train,]
test_y = podaci$FirmnessAv[-train]
set.seed(35)
logModelŒ∏ <- glm(formula=FirmnessAv ~ Œ∏, data=treniranje, family = "binomial")
summary(logModelŒ∏)

#predikcija logistiËkog modela
logpredikcija = predict(logModelŒ∏, testiranje, type = "response")
logpredY = rep("nezrela", length(test_y))
logpredY[logpredikcija > 0.5] = "zrela"
table(logpredY, test_y)

#stopa pogreöke klasifikacije
mean(logpredY != test_y)


plot(logModelŒ∏) # ode imas 4 grafa, enter u konzoli da ti pride na iduci

#logistiËka regresija pomoÊu glm funkcije Zs
set.seed(35)
logModelZs <- glm(formula=FirmnessAv ~ Zs, data = treniranje, family = "binomial")
summary(logModelZs)

#predikcija drugog logistiËkog modela
logpredikcija2 = predict(logModelZs, testiranje, type = "response")
logpredY2 = rep("nezrela", length(test_y))
logpredY2[logpredikcija2 > 0.5] = "zrela"
table(logpredY2, test_y)

#stopa pogreöke klasifikacije
mean(logpredY2 != test_y)

#odabit varijable s manjom korelacijom s ciljnom varijablom
#logistiËka regresija pomoÊu glm funkcije Zs
set.seed(35)
logModelV <- glm(formula=FirmnessAv ~ volume.cm3, data = treniranje, family = "binomial")
summary(logModelV)

#predikcija treÊeg logistiËkog modela
logpredikcija3 = predict(logModelV, testiranje, type = "response")
logpredY3 = rep("nezrela", length(test_y))
logpredY3[logpredikcija3 > 0.5] = "zrela"
table(logpredY3, test_y)

#stopa pogreöke klasifikacije
mean(logpredY3 != test_y)
