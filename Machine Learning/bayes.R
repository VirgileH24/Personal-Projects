# chargement des donn�es
nbc_ex1 <- read.delim("C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/nbc_ex1.txt")
str(nbc_ex1)
nbc_ex1

# echantillon apprentissage
train <- nbc_ex1[-11,]

# mod�lisation NBC
library(e1071)
modele_train <- naiveBayes(Y~., data=train)
modele_train # affichage probabilit�s NBC

# qualit� du mod�le
erreur_pred <- as.data.frame(predict(modele_train, train, type="raw"))
str(erreur_pred)

library(dplyr)
erreur_pred= erreur_pred %>% mutate(prev= ifelse(no > yes, "no", "yes"))
qual_modele=cbind(train$Y, erreur_pred$prev)                         
as.factor(qual_modele[,1])
as.factor(qual_modele[,2])

# erreur de mod�le
sum((qual_modele[,1] != qual_modele[,2]))/nrow(qual_modele)

# pr�diction
predict(modele_train,animaux[11,], type ="raw")


