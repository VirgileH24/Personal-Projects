# chargement des données
penguins <- read.csv("C:/Users/DELL/Dropbox/semestre 9/Intro au ML2/Base/penguins.csv", sep=";")

# analyse des données
library(summarytools)
view(dfSummary(penguins), method = "browser")

library(dplyr)
# pour utiliser l'opérateur PIPE %>% 
penguins %>% group_by(espece) %>% dfSummary() %>% view()

##################################
## Classifcation non supervisée ##
##################################

library(cluster) # algorithmes
library(factoextra) # affichage graphique des partitions

######################
# Algorithme k-means #
######################
# selection des variables quantitatives
penguins_quanti = penguins[, 2:5]

# kmeans
set.seed(75139)

fviz_nbclust(penguins_quanti, kmeans, method = "wss")

km.res <- kmeans(penguins_quanti, 3, nstart = 25)
names(km.res)

km.res$size
km.res$centers

fviz_cluster(km.res, data = penguins_quanti,
             palette = c("red", "blue", "darkgreen"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # liaison éléments uax centres de classe
             repel = TRUE, # Pas de chevauchement des libellés
             ggtheme = theme_minimal()
)

# sauvegarde de la partition
penguins_clustering <- cbind(penguins, cluster_km = km.res$cluster)
penguins_clustering$cluster_km <- as.factor(penguins_clustering$cluster_km)
str(penguins_clustering)

##################
# Algorithme PAM #
##################

# pam
fviz_nbclust(penguins_quanti, pam, method = "silhouette")+
  theme_classic()

pam.res <- pam(penguins_quanti, 3)
print(pam.res)

pam.res$medoids


fviz_cluster(pam.res,
             palette = c("red", "blue", "darkgreen"), # couleurs
             ellipse.type = "norm", # Concentration ellipse (norm, convex)
             repel = TRUE, # Pas de chevauchement des libellés
             ggtheme = theme_classic()
)

names(pam.res)
pam.res$clustering

# sauvegarde de la partition
penguins_clustering <- cbind(penguins_clustering, cluster_pam = pam.res$clustering)
penguins_clustering$cluster_pam <- as.factor(penguins_clustering$cluster_pam)
str(penguins_clustering)

# difference entre partition kmeans et PAM
penguins_clustering %>% 
  mutate(dif = 1*(cluster_km != cluster_pam)) %>% 
  group_by(dif) %>%
  summarize(n = n())

### validation de la partition
library(clustertend)
# Statistique de Hopkins
hopkins(penguins_quanti, n = nrow(penguins_quanti)-1) # valeur suggérée par Hopkins : échantillons de taille (n-1)

# matrice VAT
fviz_dist(dist(penguins_quanti), show_labels = FALSE,
          gradient = list(low = "red", mid = "white", high = "blue")) +
  labs(title = "Matrice VAT")

########################################################
# classification en utilisant l'ensemble des variables #
########################################################
penguins_quanti_quali = penguins[, -7] # suppression donnée année
str(penguins_quanti_quali)
penguins_quanti_quali$island <- as.factor(penguins_quanti_quali$island)
penguins_quanti_quali$sex <- as.factor(penguins_quanti_quali$sex)
penguins_quanti_quali$espece <- as.factor(penguins_quanti_quali$espece)
str(penguins_quanti_quali)

# matrice de dissimilarité de Gower
mat_diss= daisy(penguins_quanti_quali, metric = "gower", stand= TRUE)
as.matrix(mat_diss)      

# sélection du nombre de classes
# On ne peut pas utiliser la fonction fviz_nbclust car les variables
# sont quantitatives et qualitatives d'où une "boucle"
ind=c()
sil=c()
for(k in 2:10){
pam.res <- pam(mat_diss, k, diss= TRUE)
ind[k-1]=k-1
sil[k-1]=pam.res$silinfo$avg.width
print(paste("nb de classes = ", k, " silhouette moyenne = ", 
                     round(pam.res$silinfo$avg.width, digits = 3)))
}

ind_sil=as.data.frame(cbind(ind, sil))
ind_sil$ind = as.numeric(ind_sil$ind)
ind_sil$sil = as.numeric(ind_sil$sil)
str(ind_sil)

# graphique silhouette moyenne versus nb de classes
ggplot(ind_sil) +
  aes(x = ind, y = sil) +
  geom_point(size = 2, colour = "#0c4c8a") + geom_line()+
  labs(x = "Nombre de classes", y = "Silhouette moyenne", title = "Aide à la sélection du nombre de classes") +
  theme_gray()

# classification optimale
pam.best <- pam(mat_diss, 5, diss= TRUE)
pam.best$silinfo$clus.avg.widths
ind_pam=pam.best$medoids
penguins[ind_pam,]

# sauvegarde de la classification
penguins_clustering <- cbind(penguins_clustering, cluster_pam_best = pam.best$clustering)
penguins_clustering$cluster_pam_best <- as.factor(penguins_clustering$cluster_pam_best)
str(penguins_clustering)

# caractérisation de la partition
# comparaison des résultats de classification
library(esquisse)
esquisser(penguins_clustering)

