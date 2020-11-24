attach(GermanCredit)

#tribamo skuzit koji parametri su vazni za otkrivanje kriticnih ljudi
dim(GermanCredit) #dimenzije dataseta
summary(GermanCredit) #statistièki podaci o skupu, za svaki stupac 
head(GermanCredit)

library(corrr)

korelacije=cor(GermanCredit)
korelacije[,"RESPONSE"] #korelacije s ciljnom varijablom
# i nisu neke korelacije, najveæe su redon:
# CHK_ACC (0.350847483), DURATION (-0.214926665), HISTORY (0.228784733),
# AMOUNT (-0.154738641), SAV_ACCT (0.178942736),
# EMPLOYMENT (0.116002036), REAL_ESTATE (0.119299516), PRO_UNKN_NONE (-0.125750044)
# OTHER_INSTALL (-0.113285167), OWN_RES (0.134588776)

library(dplyr) 
# sad idemo izbacit iz dataseta one koje nam ne tribaju,
# možemo i u modelu navest konkretno varijable a možemo i izbacit iz skupa,
# ja san izabrala izbacit

#ode selectan sve "važne" varijable
GermanCredit = GermanCredit %>%
  select(CHK_ACCT, DURATION, HISTORY, AMOUNT, SAV_ACCT, 
         EMPLOYMENT, REAL_ESTATE, PROP_UNKN_NONE, OTHER_INSTALL, OWN_RES,
         RESPONSE)
head(GermanCredit, 4)
dim(GermanCredit) #imamo 10 nezavisnih varijabli i jednu ciljnu

#vizualizacija

#odnos izmeðu chec_acc i response
podaci = data.frame(CHK_ACCT, RESPONSE)
boja=c("coral3", "chartreuse3")
mosaicplot(table(podaci), color = boja, main="Odnos izmeðu varijable checking account i credit ratinga")

#employment i response
podaci1 = data.frame(EMPLOYMENT, RESPONSE)
mosaicplot(table(podaci1), color = boja, main="Odnos izmeðu varijable employment i credit ratinga")

#odnos izmeðu duration/history sa response
plot1 <- ggplot(data = GermanCredit, aes(DURATION,HISTORY,colour=RESPONSE)) + geom_point()
plot1

library(plotly)

#utjecaj amount i chk_acct varijable na response varijablu
GermanCredit$RESPONSE = as.factor(GermanCredit$RESPONSE)
str(GermanCredit)
fig = plot_ly(data = GermanCredit, x=~AMOUNT, y=~CHK_ACCT, color = ~RESPONSE, colors = boja)
fig


#distribucije klase
postotak = prop.table(table(RESPONSE))*100
postotak
cbind(frekvencija=table(RESPONSE), postotak=postotak)

#kreiranje skupova za treniranje i testiranje
library(caret)
set.seed(1)
indeksi = sample(1:nrow(GermanCredit), 0.8*nrow(GermanCredit))
treniranje = GermanCredit[indeksi,]
testiranje = GermanCredit[-indeksi,]
test_y = GermanCredit$RESPONSE[-indeksi]


#LOGISTIÈKI MODEL
library(ISLR)
logisticki = glm(formula = RESPONSE~., family = "binomial",
                 data = treniranje)
summary(logisticki)
# ovako, objasnit cemo summary
# koeficijent varijable ako je jednak nuli variajbla nije znaèajna
# ako je veci od nule znaci da kako vrijednost te varijable raste tako raste i vjerojatnost da je response veci, tj.1
# a ako je manji, tj.. negativan, onda vridi obrnuto
# u našem summary-u vidimo da su statisticki znacajne varijable redon:
# chk_acct, duration, history i sav_acc najvise (imaju 3 zvjezdice)
# i employment isto sa 2 zvjezdice

#predikcija logistièkog modela
predikcija_log = predict(logisticki, testiranje, type = "response")
predikcija_log_y = rep("0", length(test_y))
predikcija_log_y[predikcija_log > 0.5] = "1"

#matrica konfuzije
table(predikcija_log_y, test_y)

#stopa pogreške klasifikacije
mean(predikcija_log_y!= test_y)

# STABLO ODLUKE
library(rpart)
library(rpart.plot)

stablo = rpart(RESPONSE~CHK_ACCT+DURATION+HISTORY+SAV_ACCT+EMPLOYMENT, 
                data = treniranje, method = 'class')
rpart.plot(stablo, extra = 106)
# prvi èvor ti pokazuje da 71% tih kredita "dobro"
# onda gledas da li je chk_acct manji od 2, ako nije dobar je a ako je:
# 53% njih je sa manje od 2 chk_acct sa vjerojatnosti da je kredit dobar od 0.57
# onda gledas duration da li je veci od 23, i ajmo uzet da je
# ima 23% chk_acct-a manjih od 2 i sa duration vecin od 23 i vjerojatnost je 0.44 da je kredit dobar
# itd. dok ne dodes do kraja neke odredene grane, znaci postotak koji stoji doli
# pokazuje koliki je postotak takvih opservacija ukupno i skupu podataka
# a broj u sredini ti pokazuje vjerojatnost da je kredit dobar, logicno 
# ako je manji srednji broj od 0.5 da je kvadrat 0, a ako je veæi da je 1 
# to je taj gornji broj

#predikcija stabla odluke
predikcija_stablo = predict(stablo, testiranje, type = 'class')

#matrica konfuzije
matrica <- table(test_y, predikcija_stablo)
matrica

#test toènosti
test_tocnosti = sum(diag(matrica)) / sum(matrica)
print(paste('Toènost modela iznosi:', test_tocnosti))

#vjerojatnosti
tree_vj=predict(stablo, newdata=testiranje, type="prob")
head(tree_vj, 5)

#ROC krivulja
library(pROC)
roc_stablo = roc(test_y,tree_vj[,"1"])
#plot the ROC curve
plot(roc_stablo,col="blue")

#izraèun podruèja "ispod" krivulje - veæe je bolje
auc(roc_stablo)

#sluèajne šume

#kontrola - cv
cv_kontrola = trainControl(method="repeatedcv", number = 10,
                            allowParallel=TRUE)
#metoda bagging - stablo
stablo_bagg = train(RESPONSE ~ ., data=treniranje, method="treebag",
                    trControl=cv_kontrola, importance=TRUE)
stablo_bagg
#vidimo toènost od približno 0.75 ouuuu jes

#važnost varijabli - plot
plot(varImp(stablo_bagg))

#predikcija bagg stabla
bagg_predikcija = predict(stablo_bagg, newdata = testiranje, type="raw")
confusionMatrix(test_y,bagg_predikcija) #dobijemo toènost od 77% :)

#ROC krivulja i za bagg
bagg_vj=predict(stablo_bagg, newdata=testiranje,type="prob")
roc_bagg <- roc(test_y,bagg_vj[,"1"])
plot(roc_bagg,col="orange")
auc(roc_bagg)

#SLUÈAJNE ŠUME
#mozemo koristit i randomForest paket, al cu s istin ovin
suma_model <- train(RESPONSE ~ ., data=treniranje, method="rf",
                  trControl=cv_kontrola, importance=TRUE)
suma_model
#dobijemo najveæu toènost sa mtry = 6 - broj uzoraka koji se koristi za cijepanje svakog èvora

#predikcija
predikcija_rf = predict(suma_model, newdata = testiranje, type="raw")
confusionMatrix(test_y, predikcija_rf) #toènost od ~78%

#ROC krivulja
rf_vj = predict(suma_model,newdata=testiranje,type="prob") #predikcija vjerojatnosti
roc_rf = roc(test_y,rf_vj[,"1"])
plot(roc_rf,col="red")
auc(roc_rf) #ukupno podruèje

#BOOSTING SLUÈAJNA ŠUMA

library(gbm)
# metode koje se mogu koristiti su "ada", "gbm" i "xgbLinear"
# ovi algoritmi minjaju težine odreðenih varijabli (znaèaj za predikciju)
#izabrat æemo gradient boosting jer èesto ima bolje performanse od ostalih algoritama

boost_model = train(RESPONSE ~ .,data=treniranje,method="gbm",
                   verbose=F,trControl=cv_kontrola)
boost_model

#predikcija boosting modela
gbm_predikcija = predict(boost_model, newdata = testiranje, type="raw")
confusionMatrix(test_y,gbm_predikcija)
gbm_vj=predict(boost_model,newdata=testiranje,type="prob")
roc_gbm <- roc(test_y,gbm_vj[,"1"])
plot(roc_gbm, col="green")
auc(roc_gbm)

#usporedba modela
plot(roc_stablo,col="blue", main = "Usporedba modela") #plava
plot(roc_bagg,add=TRUE,col="orange") # naranèasta
plot(roc_rf,add=TRUE,col="red") # crvena
plot(roc_gbm,add=TRUE,col="green") # zelena
legend("bottomright", inset=.06, title="Tip modela",
       c("stablo odluke","bagging model","model sluèajne šume", 
         "boosting model"), fill=c("blue", "orange", "red", "green"),
       horiz=FALSE, bty = 'n', y.intersp = 1, pt.cex = 0.5, cex = 0.6)

