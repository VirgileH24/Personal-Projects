#######################
# lecture des données #
#######################
Hitters=read.table("C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/Hitters.csv", h=T, sep=",")
str(Hitters)
Hitters =na.omit(Hitters )

#analyse des données
summary(Hitters)
library(summarytools)
print(dfSummary(Hitters), method = 'viewer')

library(lattice)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  text(0.5, 0.5, txt,cex=2)
}
pairs(Hitters,lower.panel=panel.cor,panel=panel.smooth)

##################
# regression MCO #
##################
mod_MCO = lm(Salary ~ ., data=Hitters)
summary(mod_MCO)#MCO
par(mfrow = c(2,2))
plot(mod_MCO, las=1)


####################
# regression Ridge #
####################
x=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

library (glmnet )
grid =10^ seq (10,-2, length =100)
ridge.mod =glmnet (x,y,alpha =1, lambda =grid) # alpha = 0 (Ridge), alpha = 1 (LASSO)
par(mfrow = c(1,1))
plot(ridge.mod)
grid()
dim(coef(ridge.mod ))
coef(ridge.mod)[,1] # modele 1 pour lambda = 10**10
sqrt(sum(coef(ridge.mod)[ -1 ,1]^2) ) #penalité

# affichage coefficients pour valeur de lambda
ridge.mod$lambda[50] # valeur de lambda
coef(ridge.mod )[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,1]^2) )


# pour toutes les valeurs de lambda
coef(ridge.mod)[ -1 ,]^2
l2_norm=colSums(coef(ridge.mod)[ -1 ,]^2)
par(mfrow = c(1,1))
plot(l2_norm)

#echantillon apprentissage et test
set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]

ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =grid ,thresh =1e-12) #convergence 1e-12, alpha = 0 (Ridge), alpha = 1 (Lasso)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,]) #coeff pour lambda = 4
mean((ridge.pred -y.test)^2) #Erreur quadratique moyenne

#EQM sur Y et EQM sur grande valeur de lambda (quand lambda grand, estimateurs sauf constante = 0) estimation =constante
#mean((mean(y[train ])-y.test)^2)
ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

#NB : MCO obtenu avec lambda = 0 (s=0)
# choix de lambda avec validation croisée
set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
best_lambda =cv.out$lambda.min
best_lambda

#calcul regression RIDGE avec valeur optimisée de lambda
ridge.pred=predict(ridge.mod ,s=best_lambda, newx=x[test ,])
mean(( ridge.pred -y.test)^2) #SCR
ridge.mod =glmnet (x,y,alpha =0, lambda =best_lambda)
coef(ridge.mod)

ridge_fit = predict(ridge.mod,x,s=best_lambda,type="response")
MCO_fit=mod_MCO$fitted.values
indice=seq(1:length(y))
out=data.frame(indice, y, MCO_fit,ridge_fit)
plot(out$indice,out$y, main="dsffds")
points(out$indice,out$MCO_fit, col="red", pch=19)
points(out$indice,out$X1, col="blue", pch=17)

plot(out$y,out$MCO_fit, main="ajustement données modèles MCO, Ridge", col="red", pch=19, ylim=c(0,2500))
points(out$y,out$X1, pch=17, col="blue")
abline(0,1)

#LASSO
#alpha = 1

###################
# Régression MARS #
###################
library(earth)
x = Hitters[,-19]
y = Hitters[,19]

mod_deg_1 <- earth(x,y,deg=1)
summary(mod_deg_1)
mod_deg_2 <- earth(x,y,deg=2)
summary(mod_deg_2)
mod_deg_3 <- earth(x,y,deg=3)
summary(mod_deg_3)

# comparaison des modèles
par(mfrow = c(1,1))
plot.earth.models(list(mod_deg_1,mod_deg_2,mod_deg_3))
plot.earth.models(list(mod_deg_1,mod_deg_2,mod_deg_3),which=1)
grid()
plot.earth.models(list(mod_deg_1,mod_deg_2,mod_deg_3),which=2)
grid()
ev <- evimp(mod_deg_2, trim=T)
plot(ev)
print(ev)

# PDP modèle sélectionné
plotmo(mod_deg_2,ylim=NA)
mars_fit=mod_deg_2$fitted.values


######################################
# Comparaison méthodes de régression #
######################################
# valeurs observées et prédites
plot(y, mod_MCO$fitted.values, main="ajustement données modèles MCO, Ridge", col="red", pch=19, ylim=c(0,2500))
points(y,mars_fit, pch=19, col="blue")
abline(0,1)
grid()
legend(200, 2300, legend=c("MCO", "MARS"),
       col=c("red", "blue"), pch=19 , cex=1, lwd=3)

# résidus
par(mfrow = c(1,2))
boxplot(mod_MCO$residuals, col="red", ylim=c(-1000,1000))
boxplot(b$residuals, col="blue", ylim=c(-1000,1000))
grid()
