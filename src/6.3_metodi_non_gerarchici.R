# Configurazione dell’ambiente di sviluppo
df2019 <- read.csv("dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

regioni <- rownames(df2019)

parcheggio <- df2019[[1]]
names(parcheggio) <- regioni

trasporto_pubblico <- df2019[[2]]
names(trasporto_pubblico) <- regioni

traffico <- df2019[[3]]
names(traffico) <- regioni

illuminazione_stradale <- df2019[[4]]
names(illuminazione_stradale) <- regioni

condizioni_stradali <- df2019[[5]]
names(condizioni_stradali) <- regioni

# Kmeans

df_scaled <- scale(df2019)

# Scelta casuale dei punti di riferimento

for (number_of_clusters in 2:6) {
  print(paste("Numero di cluster:", number_of_clusters))

  kmeans <- kmeans(df_scaled, center=number_of_clusters, iter.max=50, nstart=50)
  print("Informazioni relative all'applicazione del metodo kmeans:")
  str(kmeans)

  print(paste("Rapporto tra la misura di non omogeneità between e quella di non omogeneità statistica totale:", kmeans$betweenss/kmeans$totss))

  pairs(df2019, col=kmeans$cluster, main = "Metodo non gerarchico del kmeans\nScelta casuale dei punti di riferimento")
}

# Scelta dei centroidi come punti di riferimento

matrice_distanze2 <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)^2
hls <- hclust(matrice_distanze2, method = "centroid")

for (number_of_clusters in 2:6) {
  print(paste("Numero di cluster:", number_of_clusters))

  taglio <- list(cutree(hls , k = number_of_clusters))
  centroidi_iniziali <- aggregate(df_scaled, taglio , mean)[,-1]
  centroidi_iniziali

  kmeans <- kmeans(df_scaled, center=centroidi_iniziali, iter.max=50)
  print("Informazioni relative all'applicazione del metodo kmeans:")
  str(kmeans)

  print(paste("Rapporto tra la misura di non omogeneità between e quella di non omogeneità statistica totale:", kmeans$betweenss/kmeans$totss))

  pairs(df2019, col=kmeans$cluster, main = "Metodo non gerarchico del kmeans\nScelta dei centroidi come punti di riferimento")
}


