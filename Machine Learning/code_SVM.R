set.seed (1234)
library (e1071)

# library(ggplotgui)
# ggplot_shiny(mes_donnees)


data = read.csv(file = "C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/swiss_roll.csv",sep = ";")

#######
# SVM #
#######

# données simulées
x=matrix (rnorm (20*2) , ncol =2)
y=c(rep (-1,10) , rep (1 ,10) )
x[y==1 ,]= x[y==1,] + 1
plot(x, col =(3-y), lwd=2, pch=16)
grid()

# codage reponse en factor
dat=data.frame(x=x, y=as.factor (y))

#modèle SVM
svmfit =svm(y~., data=dat , kernel ="linear", cost =0.1, scale =FALSE )
plot(svmfit, dat, col=c("#F9C6D7", "#3CA2C8"))

#identification des points SV
svmfit$index
summary (svmfit )

#visualisation des SV et des frontières
plot(x[,1], x[,2])
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = -svmfit$rho
abline(-beta0 / beta[2], -beta[1] / beta[2])
abline((-beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((-beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

#influence de la valeur du cout
svmfit =svm(y~., data=dat , kernel ="linear", cost =0.1, scale =FALSE )
plot(svmfit , dat, symbolPalette = rainbow(4))
svmfit$index

# validation croisée (10 échantillons k-fold)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)
bestmod =tune.out$best.model
summary(bestmod )
plot(bestmod, dat, col=c("#F9C6D7", "#3CA2C8"))

#jeu de données test
xtest=matrix (rnorm (20*2) , ncol =2)
ytest=sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame (x=xtest , y=as.factor (ytest))

ypred=predict(bestmod ,testdat)
table(predict =ypred , truth= testdat$y )

####################################
# données simulées avec 2 facteurs #
####################################

# simulation
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep (1 ,150) ,rep (2 ,50) )
dat=data.frame(x=x,y=as.factor (y))
plot(x, col=y)
grid()
# modelisation SVM
train=sample (200 ,100)
svmfit =svm(y~., data=dat [train ,], kernel ="radial", gamma =1, cost =1)
plot(svmfit , dat[train ,])
summary (svmfit)

svmfit =svm(y~., data=dat[train ,], kernel ="radial",gamma =1, cost=1e5)
plot(svmfit ,dat [train ,])

# optimisation
tune.out=tune(svm , y~., data=dat[train ,], kernel ="radial",
                ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), 
                             gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

table(true=dat[-train,"y"], pred=predict(tune.out$best.model ,
                                           newdata =dat[-train ,]))

plot(tune.out, transform.x = log10, xlab = expression(log[10](gamma)), ylab = "C")

bestGamma <- tune.out$best.parameters[[1]]
bestC <- tune.out$best.parameters[[2]]

best_svmfit =svm(y~., data=dat[train ,], kernel ="radial",gamma =bestGamma, cost=bestC)
plot(best_svmfit ,dat [train ,])


####################################
# données simulées avec 3 facteurs #
####################################

x=rbind(x, matrix (rnorm (50*2) , ncol =2))
y=c(y, rep (0 ,50) )
x[y==0 ,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor (y))
plot(x,col =(y+1))

svmfit =svm(y~., data=dat , kernel ="radial", cost =10, gamma =1)
plot(svmfit , dat, col=c("#F9C6D7", "#3CA2C8", "#10559A"))


######################################
# Avec vrai donnée
########################################

data = read.csv(file = "C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/swiss_roll.csv",sep = ";")

library(caret)

set.seed(222)

sample <- sample.int(n = nrow(CO2),
                     size = floor(.70*nrow(CO2)), # Selecting 70% of data
                     replace = F)

train <- data[sample, ]
test  <- data[-sample, ]

y_train = train$class
y_test = test$class
X_train = train[,1:2]
X_test = test[,1:2]


svmfit =svm(y~., data=train, kernel ="linear", gamma = 1, cost = 1,na.action=na.omit)
plot(svmfit , train)
summary (svmfit)


y_pred=predict(svmfit ,X_test)
table(predict = y_pred , truth= y_test )
