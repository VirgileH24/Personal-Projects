set.seed (1)

# data
swiss_roll <- read.csv("C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/swiss_roll.csv", sep=";")
str(swiss_roll)

# visualisation des données
library(ggplot2)
ggplot(swiss_roll) +
  aes(x = x, y = y, colour = class) +
  geom_point(size = 3L) +
  scale_color_distiller(palette = "Spectral") +
  labs(x = "X", y = "Y", title = "Swiss Roll") +
  theme_gray()

library(esquisse)
esquisser(swiss_roll)
plot(swiss_roll$x, swiss_roll$y, col=swiss_roll$class)
# échantillon apprentissage et test
n=nrow(swiss_roll)
pct_learning=0.8

p=ncol(swiss_roll)-1

#création échantillon d'apprentissage et échantillon test 
v = c("L","T")
s = sample(v,n,prob=c(pct_learning,1-pct_learning),replace=TRUE)
data = cbind(swiss_roll,s)

learning = subset(data, s == "L") #echantillon d'apprentissage
test = subset(data, s == "T") #echantillon test
learning = learning[,-4]
test = test[,-4]

str(learning)
str(test)

####################
# modélisation SVM #
####################
library (e1071)

# SVM linéaire
svm_lineaire =svm(class~., data=learning, kernel ="linear", type= "C-classification")
plot(svm_lineaire , learning)
summary(svm_lineaire)

pred_svm_lineaire=predict(svm_lineaire ,test)
mat_confusion_svm_lineaire=table(predict =pred_svm_lineaire , truth= test$class )
acc_svm_lineaire=sum(diag(mat_confusion_svm_lineaire))/sum(colSums(mat_confusion_svm_lineaire))
acc_svm_lineaire

# SVM non linéaire (Gaussien)
svm_non_lineaire =svm(class~., data=learning, kernel ="radial", gamma=1/p, type ="C-classification")
plot(svm_non_lineaire , learning)
summary(svm_non_lineaire)

pred_svm_non_lineaire=predict(svm_non_lineaire ,test,  decision.values = TRUE)
mat_confusion_svm_non_lineaire=table(predict =pred_svm_non_lineaire , truth= test$class )
acc_svm_non_lineaire=sum(diag(mat_confusion_svm_non_lineaire))/sum(colSums(mat_confusion_svm_non_lineaire))
acc_svm_non_lineaire

####################
# modélisation RKF #
####################
p=2
gamma=1/p
c=2

X_train=as.matrix(learning[,-3])
n1=nrow(X_train)
Y_train=as.matrix(learning[,3])
X_test=as.matrix(test[,-3])
n2=nrow(X_test)
Y_test=as.matrix(test[,3])

W=matrix(rnorm(p*c,mean=0,sd=gamma), p, c) 
b=runif(c,min=0,max=2*pi)

b_train=matrix(b, n1, c, byrow = TRUE) 
Z_train=sqrt(2/c)*cos(X_train %*% W +b_train)
RKF_train=as.data.frame(cbind(Z_train, Y_train))

b_test=matrix(b, n2, c, byrow = TRUE) 
Z_test=sqrt(2/c)*cos(X_test %*% W +b_test)
RKF_test=as.data.frame(cbind(Z_test, Y_test))
str(RKF_test)

# SVM linéaire
svm_lineaire_RKF =svm(RKF_train[,3]~., data=RKF_train, kernel ="linear", type ="C-classification")
summary(svm_lineaire_RKF)

pred_svm_lineaire_RKF=predict(svm_lineaire_RKF , RKF_test,  decision.values = TRUE)
mat_confusion_svm_lineaire_RKF=table(predict =pred_svm_lineaire_RKF , truth= test$class )
acc_svm_lineaire_RKF=sum(diag(mat_confusion_svm_lineaire_RKF))/sum(colSums(mat_confusion_svm_lineaire_RKF))
acc_svm_lineaire_RKF


?plot.svm
