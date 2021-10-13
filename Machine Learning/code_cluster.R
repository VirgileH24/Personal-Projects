# librairies
library(cluster)
library(factoextra)

###########
# données #
###########
set.seed(1234)
data("USArrests")
df <- scale(USArrests) # centrage et reduction des données
head(df, n = 3)

####################
# methodes k-means #
####################
# selection du nombre optimal de classe
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# K-MEANS
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)

# affichage des centres de classes 
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

km.res$cluster # affection des classes par individu
km.res$size # taille des classes
km.res$centers # centre de classes

# affichage de la partition sur plan factoriel
fviz_cluster(km.res, data = df,
palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
ellipse.type = "euclid", # Concentration ellipse
star.plot = TRUE, # Ajouts segments des éléments aux centres de classes
repel = TRUE, # Pas de chevauchement des libellés
ggtheme = theme_minimal()
)

###############
# méthode PAM #
###############
# selection du nombre optimal de classe
fviz_nbclust(df, pam, method = "silhouette")+ theme_classic()

# PAM
pam.res <- pam(df, 2)
print(pam.res)

# affection des invidus aux classes
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 10)

# médoïdes
pam.res$medoids
head(pam.res$clustering)

# affichage de la aprtition sur le plan factoriel
fviz_cluster(pam.res,
palette = c("#00AFBB", "#FC4E07"), # coleurs
ellipse.type = "t", # Concentration ellipse
repel = TRUE, # Pas de chevauchement des libellés
ggtheme = theme_classic()
)

#################
# méthode CLARA #
#################

# génération de 500 individus répartis en 2 classes
df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
cbind(rnorm(300,50,8), rnorm(300,50,8)))

# affection des noms aux colonnes et aux individus de la dataframe
colnames(df) <- c("x", "y")
rownames(df) <- paste0("S", 1:nrow(df))
head(df, nrow = 6)

# affichage du graphe des silhouettes
fviz_nbclust(df, clara, method = "silhouette")+
theme_classic()

# CLARA
clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)
print(clara.res)

dd <- cbind(df, cluster = clara.res$cluster) # partition des données
head(dd, n = 4)

# médoïdes
clara.res$medoids
head(clara.res$clustering, 10)

# affichage graphique de la partition
fviz_cluster(clara.res,
palette = c("#00AFBB", "#FC4E07"), # color palette
ellipse.type = "t", # Concentration ellipse
geom = "point", pointsize = 1,
ggtheme = theme_classic()
)

################################
### validation de la partition #
################################
# Test de Hopkins
library(clustertend)
hopkins(df, n = nrow(df)-1)

# Matrice VAT
fviz_dist(dist(df), show_labels = FALSE)  + labs(title = "Data")

