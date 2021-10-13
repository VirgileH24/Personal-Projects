set.seed(1)
#######################
# importation données #
#######################
Boston <- read.delim("~/Ecoles/2020-2021/Dauphine/ML/BaggingRFBoosting/Boston.txt")
str(Boston)

########
# CART #
########
library(rpart)
library(rpart.plot)#affichage des arbres
library(RColorBrewer)
library(rattle)

tree = rpart(MV~., data = Boston)
tree$cptable

plotcp(tree)

best_tree=prune(tree, 0.016) # critere 1SE
rpart.plot(best_tree,extra=1, type=4,tweak=1.5)
names(best_tree)

###########
# Bagging #
###########
library(randomForest)
bag=randomForest(MV~., data=Boston, ntree=500, mtry=13, importance=TRUE)
bag
names(bag)
bag$importance
varImpPlot(bag)

plot(Boston$MV,bag$predicted, pch=20)
abline(0,1)
grid()

bag$importance

#################
# Random Forest #
#################
rf=randomForest(MV~., data=Boston, ntree=500, importance=TRUE)
rf
names(rf)
varImpPlot(rf)
rf$mtry

#####################
# gradient boosting #
#####################
library(gbm)
boost=gbm(MV~., data=Boston, n.trees=5000, interaction.depth=2, shrinkage=0.1)
summary(boost, las = 1)
boost
plot(boost, i.var = "LSTAT")
plot(boost, i.var = "RM")
names(boost)

par(mfrow=c(1,1))

plot(Boston$MV, bag$predicted, pch=19, col="red", main="Comparaison Bagging, Random Forest et Gradient Boosting")
points(Boston$MV, rf$predicted, pch=19, col="blue")
points(Boston$MV, boost$fit, pch=19, col="green")
abline(0,1)
grid()
legend(5, 45, legend=c("Bagging", "Random Forest", "GBM"),
       col=c("red", "blue", "green"), pch=19 , cex=1, lwd=3)



