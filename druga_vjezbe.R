attach(edukacija88)
library(arules)
library(tree)
library(ISLR)
library(data.table)

#izostavljanje varijable stog i id
setDT(edukacija88)[, stog :=NULL]
setDT(edukacija88)[, id :=NULL]
edukacija88

#diskretizacija dataset-a
binarni=edukacija88[,6:13]
nebinarni=edukacija88[,1:5]
eduDisk=discretizeDF(nebinarni,default = list(method="interval",breaks=3,labels=c("bad","good","excellent")))
edudis=data.frame(eduDisk, binarni)
head(edudis)
attach(edudis)

#inicijalno stablo odluke
tree.edu=tree(grade~.-grade, edudis)
summary(tree.edu)
plot(tree.edu)
text(tree.edu,pretty=0)
tree.edu

#procjena greške testiranja
set.seed(4)
ucenje=sample(1:nrow(edudis), 43)
test=edudis[-ucenje,]
test_grade=grade[-ucenje]
tree.edu=tree(grade~.-grade,edudis,subset=ucenje)
tree.pred=predict(tree.edu,test,type="class")
table(tree.pred,test_grade)
(10+22)/34

#cross validacija
set.seed(4)
cv.edu=cv.tree(tree.edu, FUN = prune.misclass)
names(cv.edu)
cv.edu
par(mfrow=c(1,2))
plot(cv.edu$size,cv.edu$dev,type="b")
plot(cv.edu$k,cv.edu$dev,type="b")

#orezivanje stabla
prune.edu=prune.misclass(tree.edu, best=3)
plot(prune.edu)
text(prune.edu, pretty = 0)
tree.pred=predict(prune.edu,test,type="class")
table(tree.pred,test_grade)
(10+22)/34

#orezivanje stabla na veæu od najbolje vrijednosti
prune.edu2=prune.misclass(tree.edu2,best=5)
tree.pred3=predict(prune.edu2,test,type="class")
table(tree.pred3,test_grade)
(7+23)/34
