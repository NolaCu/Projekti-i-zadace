#da bi mogli izvest funkciju attach i pokrenuti skriptu potrebno je obaviti import baze "edukacija2"
attach(Edukacija2)
names(Edukacija2)
library(ggplot2)
library(gplots)

#lectures
sredina_lec=round(tapply(lectures, grade, mean), digits=2)
sredina_lec
plotmeans(lectures~grade, digits=2, ccol="red", mean.labels = T, main="Graf srednje vrijednosti bodova iz aktivnosti")
boxplot(lectures~grade, main="Srednja vrijednost bodova iz aktivnosti prema ocjeni", xlab="ocjena", ylab="Bodovi iz aktivnosti", col=rainbow(7))
points(sredina_lec, col="black", pch = 18)
aov_lectures=aov(lectures~grade)
summary(aov_lectures)
tuk=TukeyHSD(aov_lectures)
tuk
plot(tuk)

#selfassessments
sredina_self=round(tapply(selfassessments, grade, mean), digits=2)
sredina_self
plotmeans(selfassessments~grade, digits=2, ccol="blue", mean.labels = T, main="Graf srednje vrijednosti klikova samoprovjere")
boxplot(selfassessments~grade, main="Srednja vrijednost klikova u okviru samoprovjere prema ocjeni", xlab="ocjena", ylab="Klikovi u okviru samoprovjere", col=rainbow(7))
points(sredina_self, col="black", pch=20)
aov_self=aov(selfassessments~grade)
summary(aov_self)
tuk_self=TukeyHSD(aov_self)
tuk_self
plot(tuk_self)

#videos
sredina_video=round(tapply(videos, grade, mean), digits=2)
sredina_video
plotmeans(videos~grade, digits=2, ccol="green", mean.labels = T, main="Graf srednje vrijednosti pokretanja snimljenih predavanja")
boxplot(videos~grade, main="Srednja vrijednost broja pokretanja snimku predavanja prema ocjeni", xlab="ocjena", ylab="Broj pokretanja snimljenih predavanja", col=rainbow(7))
points(sredina_video, col="black", pch=20)
aov_videos=aov(videos~grade)
summary(aov_videos)
tuk_video=TukeyHSD(aov_videos)
tuk_video
plot(tuk_video)

#quizzes
sredina_quiz=round(tapply(quizzes, grade, mean), digits=2)
sredina_quiz
plotmeans(quizzes~grade, digits=2, ccol = "violet", mean.labels = T, main="Graf srednje vrijednosti ukupnih bodova ostvarenih na kvizovima")
boxplot(quizzes~grade, main="Srednja vrijednost ukupnog broja bodova ostvarenih u kvizovima prema ocjeni", xlab="ocjena", ylab="Ukupan broj bodova ostvarenih na oba kviza", col=rainbow(3))
points(sredina_quiz, col="black", pch=20)
aov_quiz=aov(quizzes~grade)
summary(aov_quiz)
tuk_quiz=TukeyHSD(aov_quiz)
tuk_quiz
plot(tuk_quiz)

#labs
sredina_lab=round(tapply(Edukacija2$labs, grade, mean), digits=2)
sredina_lab
plotmeans(Edukacija2$labs~grade, digits=2, ccol="#F08080", mean.labels = T, ylab="labs", main="Graf srednje vrijednosti bodova na vježbama")
boxplot(Edukacija2$labs~grade, main="Srednja vrijednost ukupnog broja bodova s vježbi prema ocjeni", xlab="ocjena", ylab="Ukupan broj bodova s vježbi", col=rainbow(3))
points(sredina_lab, col="black", pch=20)
aov_labs=aov(Edukacija2$labs~grade)
summary(aov_labs)
tuk_lab=TukeyHSD(aov_labs)
tuk_lab
plot(tuk_lab)
