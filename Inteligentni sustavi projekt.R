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

#ureðivanje i spajanje skupova podataka
str(data1)
str(data2)
data1 = data1[,-c(1,2,3,4,5,6,7,8,9,11,13,14,15,16,18, 19,23)]
str(data1)
data2 = data2[, -c(1,2,3,4,5,7,9,10,11,12,14,15)]
str(data2)
podaci=rbind(data1,data2)
str(podaci)

#zaokruživanje podataka na decimale zbog razlièitih vrijednosti u skupovima
round(podaci, digits=2)

#ispitujemo korelaciju varijabli kako bi odabrali ispravne parametre za izradu analize
cor(podaci)

#1.zadatak - vizualizacija skupa podataka
ggplot(podaci, aes(x=volume.cm3, y=density.g.cm3))+geom_point(colour="orange")
ggplot(podaci, aes(x=Zs, y=FirmnessAv))+geom_point(color="orange")
ggplot(podaci, aes(x=Î¸, y=FirmnessAv))+geom_point(colour="orange")

#histogram
hist(FirmnessAv, main = 'vrijednsoti varijable FirmnessAv',xlab = 'Zrelost breskve', col='orange')

#density plot
densityplot(~FirmnessAv, main="Density plot",  xlab="Zrelost breskve")

#boxplot(podaci[,0:6], main='Multiple Box plots')

plotic = plot_ly(data = podaci, x =~Zs, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
plotic = plot_ly(data = podaci, x =~Î¸, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
plotic = plot_ly(data = podaci, x =~volume.cm3, y = ~FirmnessAv, color = I("orange"), size = ~FirmnessAv, type="scatter", alpha = 0.5)
plotic
summary(podaci)

#izrada modela
#podjela podataka na skupove
set.seed(35)
treniranje = sample(1:nrow(podaci), 0.8*nrow(podaci))
testiranje = podaci[-treniranje,"FirmnessAv"]

#podešavanje parametara da se MSE smanji (u startu je bilo 0.7..)
#sluèajna šuma na temelju prve varijable - Zs
set.seed(35)
suma2=randomForest(FirmnessAv~Zs,data=podaci,subset=treniranje,importance=TRUE, 
                   ntree=994, sampsize=7, nPerm=8,# u ovom modelu dodavanjem parametra nPerm smanjuje MSE grešku
                   nodesize=1, maxnodes=8)#, do.trace=TRUE) #manji samp size - bolji rezultati

#radimo trace za praæenje greške po stablima 
#predikcija treæeg modela 
predikcija3 = predict(suma2,newdata=podaci[-treniranje,])

#graf predikcije
plot(x=predikcija3, testiranje, main="Graf predikcije prvog modela sluÄajne Å¡ume",
     xlab="predviÄ‘ena vrijednost", ylab="stvarna vrijednost", col="orange")
abline(0,1)

#MSE pogreška treæeg modela
MSE(predikcija3, testiranje)

#sluèajna šuma na temelju druge varijable 
set.seed(35)
suma4=randomForest(FirmnessAv~Î¸,data=podaci,subset=treniranje,importance=TRUE, 
                   ntree=1510, sampsize=50,
                   nodesize=1, maxnodes=8)#, do.trace=TRUE) #poveæavanjem parametra nPerm poveæavamo MSE grešku modela

#predikcija treæeg modela 
predikcija4 = predict(suma4,newdata=podaci[-treniranje,])

#graf predikcije
plot(x=predikcija4, testiranje, main="Graf predikcije drugog modela sluÄajne Å¡ume",
     xlab="predviÄ‘ena vrijednost",ylab="stvarna vrijednost", col="orange")
abline(0,1)

#MSE pogreška treæeg modela
MSE(predikcija4, testiranje)

#plot slucajne sume
plot(suma2, col="orange")# error nad podacima za uÄenje
plot(suma2$rsq, type="l", col="orange") #r2
plot(suma4, col="orange")# error nad podacima za uÄenje
plot(suma4$rsq, type="l", col="orange") #r2


#2.zadatak
importance(suma2)
varImpPlot(suma2)

#iz ove funkcije vidimo da najveæi znaèaj za predikciju zrelosti breskve imaju
#varijable Zs i Î¸
filterVarImp(podaci[1:6], podaci$FirmnessAv, nonpara = TRUE)
importance(suma4)
varImpPlot(suma4)

#logistièka regresija pomoæu glm funkcije 

#diskretizacija ciljne varijable
podaci$FirmnessAv=discretize(FirmnessAv, method = "frequency", breaks = 2,labels =c("nezrela", "zrela"))
head(podaci)

#podjela podataka
train = sample(1:nrow(podaci), 0.8*nrow(podaci))
treniranje = podaci[train,]
testiranje = podaci[-train,]
test_y = podaci$FirmnessAv[-train]
set.seed(35)
logModelÎ¸ <- glm(formula=FirmnessAv ~ Î¸, data=treniranje, family = "binomial")
summary(logModelÎ¸)

#predikcija logistièkog modela
logpredikcija = predict(logModelÎ¸, testiranje, type = "response")
logpredY = rep("nezrela", length(test_y))
logpredY[logpredikcija > 0.5] = "zrela"
table(logpredY, test_y)

#stopa pogreške klasifikacije
mean(logpredY != test_y)


plot(logModelÎ¸) # ode imas 4 grafa, enter u konzoli da ti pride na iduci

#logistièka regresija pomoæu glm funkcije Zs
set.seed(35)
logModelZs <- glm(formula=FirmnessAv ~ Zs, data = treniranje, family = "binomial")
summary(logModelZs)

#predikcija drugog logistièkog modela
logpredikcija2 = predict(logModelZs, testiranje, type = "response")
logpredY2 = rep("nezrela", length(test_y))
logpredY2[logpredikcija2 > 0.5] = "zrela"
table(logpredY2, test_y)

#stopa pogreške klasifikacije
mean(logpredY2 != test_y)

#odabit varijable s manjom korelacijom s ciljnom varijablom
#logistièka regresija pomoæu glm funkcije Zs
set.seed(35)
logModelV <- glm(formula=FirmnessAv ~ volume.cm3, data = treniranje, family = "binomial")
summary(logModelV)

#predikcija treæeg logistièkog modela
logpredikcija3 = predict(logModelV, testiranje, type = "response")
logpredY3 = rep("nezrela", length(test_y))
logpredY3[logpredikcija3 > 0.5] = "zrela"
table(logpredY3, test_y)

#stopa pogreške klasifikacije
mean(logpredY3 != test_y)
