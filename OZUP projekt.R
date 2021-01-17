library(dplyr)
library(arules)
library(ggplot2)
library(plyr)
library(DMwR)
library(corrplot)
library(lares)
library(caret)
library(xgboost)
library(glmnet)
library(pROC)
library(DALEX)

breskve = peaches_1
attach(breskve)

# izbacivanje NA vrijednosti - zadnaj 4 reda
breskve[ breskve == "NULL"] <- NA
breskve = breskve[ complete.cases(breskve),]

# izbacivanje prvog stupca - nije važan za analizu
breskve = subset(breskve, select=-X.U.FEFF.num)

# provjeravamo strukturu podataka
str(breskve)

# mijenjanje vrste varijable AE jer ne možemo dobiti korelaciju dok su kategorije
breskve$AE = as.numeric(factor(breskve$AE))

# ispitivanje korelacije između varijabli, za model možemo koristiti varijable koje imaju korelaciju veću od 0.2, a to su:
# TSS, zs, theta, color, B1, C1, WI_CIE1, L2, B2, C2, H2
korelacije = cor(breskve)
korelacije = korelacije[, "firm"]
korelacije

# vizualizacija
corr_var(breskve,firm,top = 11) 

# izvlacenje najvece korelacije

# diskretizacija ciljne varijable
breskve$firm=discretize(breskve$firm, method = "interval", breaks = 2,labels =c("zrela", "nezrela"))
head(breskve)
ggplot(breskve, aes(firm))+geom_bar(fill = "steelblue") + ggtitle("Odnos između klasa Firm variajble")
count(breskve$firm)

# SMOTE balansiranje klasa - probala san stavit sampling unutar kontrole ali su rezultati bolji ako se promjeni direktno dataset
prop.table(table(breskve$firm))
set.seed(42)
breskve$firm <- as.factor(breskve$firm)
breskve_novi <- SMOTE(firm ~ ., breskve, perc.over = 200, perc.under=150)
prop.table(table(breskve_novi$firm))

# vizualizacija novog omjera
ggplot(breskve_novi, aes(firm))+geom_bar(fill = "#f4a4d2") + ggtitle("Odnos između klasa Firm variajble nakon primjene SMOTE metode")

#kontrola = trainControl(method = "repeatedcv", repeats = 10, classProbs = TRUE,
                        #summaryFunction = twoClassSummary, sampling = "smote")

# razdvajanje data seta na skupove za treniranje i testiranje
set.seed(42)
index = createDataPartition(breskve_novi$firm, p = 0.7, list = FALSE)
treniranje = breskve_novi[index, ]
testiranje = breskve_novi[-index, ]  

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees = 150, 
                        shrinkage = 0.1, n.minobsinnode = 5)

# izrada gradient boosting modela
gradient_boosting <- train(firm ~ theta + zs + B1 + WI_CIE1+ L2 + C1 + B2 + color + C2 + TSS + H2, data = treniranje, 
                           method = "gbm", tuneGrid = gbmGrid)
summary(gradient_boosting)
predikcija = predict(gradient_boosting, newdata = testiranje)
head(predikcija)
final_smote <- data.frame(prava_klasa = testiranje$firm, predikcija)

# matrica i vizualizacija
cm_smote <- confusionMatrix(final_smote$predikcija, testiranje$firm)
cm_smote
ggplot(as.data.frame(cm_smote$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="#ffe4e1", high="#f88379") +
  labs(x = "Prava klasa",y = "Predikcija") +
  scale_x_discrete(labels=c("Zrela","Nezrela")) +
  scale_y_discrete(labels=c("Nezrela","Zrela"))


# izrada logističkog modela
set.seed(42)
logisticki_model = train(firm ~ theta + zs + B1 , data = treniranje, 
                           method = "glm", verbose = FALSE, metric = "ROC", family = "binomial", trControl = kontrola, control = list(maxit = 150))
summary(logisticki_model)

predikcija_log = predict(logisticki_model, newdata = testiranje)
head(predikcija_log)
final_logisticki <- data.frame(prava_klasa = testiranje$firm, predikcija_log)
cm_logisticki <- confusionMatrix(final_logisticki$predikcija_log, testiranje$firm)
cm_logisticki
ggplot(as.data.frame(cm_logisticki$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="#ffe4e1", high="#f88379") +
  labs(x = "Prava klasa",y = "Predikcija") +
  scale_x_discrete(labels=c("Zrela","Nezrela")) +
  scale_y_discrete(labels=c("Nezrela","Zrela"))

# usporedba modela
# roc krivulja prvog modela
boost_vj = predict(gradient_boosting,newdata=testiranje,type="prob")
roc_boost = roc(testiranje$firm,boost_vj[,"zrela"])
auc(roc_boost)

# roc krivulja drugog modela
logit_vj = predict(logisticki_model,newdata=testiranje,type="prob")
roc_logit = roc(testiranje$firm,logit_vj[,"zrela"])
auc(roc_logit)

plot(roc_boost,col="blue", main = "Usporedba modela") #plava
plot(roc_logit,add=TRUE,col="orange") # narančasta
legend("bottomright", inset=.06, title="Tip modela",
       c("Gradient Boosting model","Logistički model"), fill=c("blue", "orange"),
       horiz=FALSE, bty = 'n', y.intersp = 1, pt.cex = 0.5, cex = 0.6)

# postupci objašnjavanja modela

# break down plot boosting model
explain_boost = explain(model = gradient_boosting, data = breskve_novi[, -3],
                        y = breskve_novi$firm == "zrela", label = "Gradient Boosting model")

bd_boost = predict_parts(explainer = explain_boost, new_observation = testiranje, 
                         type = "break_down")
plot(bd_boost, max_features=5)

# break down plot logisticki
explain_log = explain(model = logisticki_model, data = breskve_novi[, -3],
                        y = breskve_novi$firm == "zrela", label = "Logistički model")

bd_log = predict_parts(explainer = explain_log, new_observation = testiranje,
                         type = "break_down")
plot(bd_log, max_features=3)

# shapley
shap_boost <- predict_parts(explainer = explain_boost, new_observation = testiranje, 
                            type = "shap", B = 25)
plot(shap_boost, show_boxplots = FALSE) 
shap_log <- predict_parts(explainer = explain_log, new_observation = testiranje, 
                            type = "shap", B = 25)
plot(shap_log, show_boxplots = FALSE, max_features = 3) 

# variable importance
set.seed(1980)
vip_boost = model_parts(explainer = explain_boost, loss_function = auc, B = 1, variables = c("theta", "zs", "B1"))
vip_log = model_parts(explainer = explain_log,  loss_function = auc, B = 1, variables = c("theta", "zs", "B1"))
plot(vip_boost, vip_log) +
  ggtitle("Mean variable-importance over 1 permutation", "")


