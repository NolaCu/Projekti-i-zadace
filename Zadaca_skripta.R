#import potrebnih bibilioteka
library(ggplot2)
library(lattice)
library(plotly)
library(yardstick)
library(caret)
#vizualizacija omjera vrijednosti varijable stog i grade
attach(edukacija90)
podaci=data.frame(stog,grade)
podaci
boja=c("red", "yellow","green")
mosaicplot(table(podaci),color=boja, main="Odnos izmeÄ‘u varijable stog i grade")

#utjecaja varijabli videos i stog na konaènu ocjenu studenta
fig=plot_ly(data=edukacija90, x=~stog, y=~videos, color=~grade, colors=boja)
fig

#utjecaj varijable labs i stog na ocjenu studenta
fig=plot_ly(data=edukacija90, x=~stog, y=~labs, color=~grade, colors=boja)
fig

#utjecaj varijabli selfassesm i stog na ocjenu studenta
fig=plot_ly(data=edukacija90, x=~stog, y=~selfassesm, color=~grade, colors=boja)
fig

#utjecaj varijabli stog i lectures na ocjenu studenta
fig=plot_ly(data=edukacija90, x=~stog, y=~lectures, color=~grade, colors=boja)
fig

#utjecaj varijabli stog i quizzes na ocjenu studenta
fig=plot_ly(data=edukacija90, x=~stog, y=~quizzes, color=~grade, colors=boja)
fig

#logistièka regresija
attach(edukacija88)

#priprema podataka za izradu modela
train=id<60
train
test=!train
treniranje=edukacija88[train,]
testiranje=edukacija88[test,]
testiranje
y=grade[test]
y

#model1 - uzima u obzir varijable stog i lectures za predikciju ocjene
library(ISLR)
model1=glm(formula=grade~lectures+stog, family="binomial", data=treniranje)
predikcija=predict(model1,testiranje,type="response")
predikcija1_y=rep("FAIL",length(ytest))
predikcija1_y
predikcija1_y[predikcija>0.5]="PASS"
predikcija1_y
prva=table(predikcija1_y,y)
prva
fourfoldplot(prva)
mean(predikcija1_y!=y)

#model2  - uzima u obzir varijable selfassesm i stog za predikciju ocjene
model2=glm(formula=grade~selfassesm+stog, family="binomial", data=treniranje)
predikcija2=predict(model2,testiranje,type="response")
predikcija2_y=rep("FAIL",length(ytest))
predikcija2_y[predikcija2>0.5]="PASS"
predikcija2_y
druga=table(predikcija2_y,y)
fourfoldplot(druga)
mean(predy2!=ytest)

#model3 - uzima u obzir varijable selfassesm, stog, labs, lectures i quizzes za predikciju ocjene
model3=glm(formula=grade~selfassesm+stog+labs+lectures+quizzes, family="binomial", data=treniranje)
predikcija3=predict(model3,testiranje,type="response")
predikcija3_y=rep("FAIL",length(ytest))
predikcija3_y[predikcija3>0.5]="PASS"
predikcija3_y
tri=table(predikcija3_y,y)
fourfoldplot(tri)
mean(predy3!=ytest)

#model4 - uuzima u obzir varijable stog i labs za predikciju ocjene
model4=glm(formula=grade~stog+labs, family="binomial", data=treniranje)
predikcija4=predict(model4,testiranje,type="response")
predikcija4_y=rep("FAIL",length(ytest))
predikcija4_y[predikcija4>0.5]="PASS"
predikcija4_y
cetvrta=table(predikcija4_y,y)
fourfoldplot(cetvrta)
mean(predy4!=ytest)

#model5 - uzima u obzir varijable stog, labs, videos, lectures, selfassesm, forum i demons za predikciju ocjene
model5=glm(formula=grade~stog+labs+videos+lectures+selfassesm+forum+demons, family="binomial", data=treniranje)
predikcija5=predict(model5,testiranje,type="response")
predikcija5_y=rep("FAIL",length(ytest))
predikcija5_y[predikcija5>0.5]="PASS"
predikcija5_y
peta=table(predikcija5_y,y)
mean(predy5!=ytest)
fourfoldplot(peta)

