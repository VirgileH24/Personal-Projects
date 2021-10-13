set.seed(123)
library(missForest)
library(VIM)
library(summarytools)

##################
# Jeu de données #
##################
# répertoire
setwd("C:/Users/s002139/Documents/Ecoles/2020-2021/Dauphine/ML/Missing Values")

# lecture données
df <- read.csv("~/Ecoles/2020-2021/Dauphine/ML/Missing Values/SouthAfricanHeart.csv", sep=";")
head(df)

pct_VM=0.1 

df_VM=prodNA(df, noNA = pct_VM)
df_VM$Class=as.factor(df_VM$Class)
summary(df_VM)

# visualisation des valeurs manquantes
view(dfSummary(df_VM, method="viewer"))
a <- aggr(df_VM, plot = FALSE)
plot(aggr(df_VM, plot = FALSE), numbers = TRUE, prop = FALSE)
marginplot(df_VM[, c("Obesity", "Age")],col = c("blue", "red"))
legend(37, 37, legend=c("Available", "Missing"),
       col=c("blue", "red"), lty=1, cex=0.8)
grid()

scattmatrixMiss(df_VM, highlight=c("Obesity", "Age"), alpha=1)

######################################
# Remplacement VM par algorithme knn #
######################################

# utilisation fonction moyenne pour estimer VM 
df_knn_avg=kNN(df_VM,k = 5, numFun = mean, catFun=maxCat)
scattmatrixMiss(df_knn_avg[,c(4,7,14,17)], 
                delimiter = "_imp",
                col=c("blue","red"), alpha=1)


marginmatrix(df_knn_avg[,c(4,7,14,17)], delimiter = "_imp", col=c("blue","red", "pink"))

df_obesity_true=subset(df, select = Obesity)
names(df_obesity_true)[1] <- c("Obesity_True")

df_obesity_imputed=subset(df_knn_avg, select=c("Obesity", "Obesity_imp"))
names(df_obesity_imputed)
names(df_obesity_imputed)[1] <- c("Obesity_Imputed")

df_obesity=cbind(df_obesity_true, df_obesity_imputed)
df_obesity=subset(df_obesity, Obesity_imp==TRUE, select = c("Obesity_True", "Obesity_Imputed"))
str(df_obesity)
hist(df_obesity$Obesity_True-df_obesity$Obesity_Imputed)

# utilisation fonction médiane pour estimer VM 
df_knn_med=kNN(df_VM,k = 5, numFun = median, catFun=maxCat)
scattmatrixMiss(df_knn_med[,c(4,7,14,17)], 
                delimiter = "_imp",
                col=c("blue","red"), alpha=1)


#############################################
# Remplacement VM par algorithme missForest #
#############################################
library(missForest)

df_mf_imp=missForest(df_VM)
df_mf= df_mf_imp$ximp
df_mf_imp$OOBerror

################
# Modelisation #
################
library(randomForest)

# modélisation à partir des VM estimées par knn avec fonction moyenne 
rf_knn_avg <- randomForest(x = df_knn_avg[,1:9], y = df_knn_avg$Class)
pred_rf_knn_avg <- predict(rf_knn_avg, df_knn_avg[,1:9], type = "prob")

# modélisation à partir des VM estimées par knn avec fonction médiane
rf_knn_med <- randomForest(x = df_knn_med[,1:9], y = df_knn_med$Class)
pred_rf_knn_med <- predict(rf_knn_med, df_knn_med[,1:9], type = "prob")

# modélisation à partir des VM estimées par missForest
rf_mf <- randomForest(x = df_mf[,1:9], y = df_mf$Class)
pred_rf_mf <- predict(rf_mf, df_mf[,1:9], type = "prob")

# test ROC pour données avec VM estimées par knn avec fonction moyenne
test_scores_knn_avg = data.frame(event_prob = pred_rf_knn_avg[,2], labels = df$Class)

# test ROC pour données avec VM estimées par knn avec fonction médiane
test_scores_knn_med = data.frame(event_prob = pred_rf_knn_med[,2], labels = df$Class)

# test ROC pour données avec VM estimées par missForest
test_scores_mf = data.frame(event_prob = pred_rf_mf[,2], labels = df$Class)

# courbes ROC
library(PRROC)
test_roc_knn_avg <- roc.curve(scores.class0 = test_scores_knn_avg[test_scores_knn_avg$labels == "1", ]$event_prob, 
                              scores.class1 = test_scores_knn_avg[test_scores_knn_avg$labels == "0", ]$event_prob, 
                              curve=T)

test_roc_knn_med <- roc.curve(scores.class0 = test_scores_knn_med[test_scores_knn_med$labels == "1", ]$event_prob, 
                              scores.class1 = test_scores_knn_med[test_scores_knn_med$labels == "0", ]$event_prob, 
                              curve=T)

test_roc_mf <- roc.curve(scores.class0 = test_scores_mf[test_scores_mf$labels == "1", ]$event_prob, 
                         scores.class1 = test_scores_mf[test_scores_mf$labels == "0", ]$event_prob, 
                         curve=T)

library(ggplot2)
library(dplyr)
data.frame(method = "knn avg", FPR = test_roc_knn_avg$curve[ ,1], TPR = test_roc_knn_avg$curve[ ,2]) %>%
  rbind(data.frame(method = "knn med", FPR = test_roc_knn_med$curve[ ,1], TPR = test_roc_knn_med$curve[ ,2])) %>%
  rbind(data.frame(method = "missForest", FPR = test_roc_mf$curve[ ,1], TPR = test_roc_mf$curve[ ,2])) %>%
  ggplot(aes(x = FPR, y = TPR, col = method)) + 
  geom_line(size=1.2) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(title = "knn avg, knn med and MissForest", 
       subtitle = paste0("knn avg AUC = ", round(test_roc_knn_avg$auc, 3), 
                         ", knn med AUC = ", round(test_roc_knn_med$auc, 3)
                         ,", missForest AUC = ", round(test_roc_mf$auc, 3)), 
       col = "Missing Values Method")

