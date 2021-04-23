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

# Metodi gerarchici agglomerativi

df_scaled <- scale(df2019)

# Metodo del legame singolo

matrice_distanze <- dist(df_scaled, method = "euclidean", diag = TRUE, upper = TRUE)

hls <- hclust(matrice_distanze, method = "single")

#str(hls)
#
#plot(hls, hang = -1, axes = FALSE, xlab = " Metodo gerarchico agglomerativo", sub = "del legame singolo")
#axis(side = 2, at = round(c(0, hls$height), 2), las = 2)

# Misure di non omogeneità statistiche

#numero_righe_dataset <- nrow(df2019)
#trHI <- (numero_righe_dataset - 1) * sum(apply(df2019, 2, var))
#print(paste("Misura di non omogeneità statistica totale:", trHI))
#
#for (number_of_clusters in 2:6) {
#  print(paste("Numero di cluster:", number_of_clusters))
#  cardinalita_gruppi <- table(cutree(hls, k = number_of_clusters))
#  taglio <- list(cutree(hls, k = number_of_clusters))
#  agvar <- aggregate(df2019, taglio, var)[, -1]
#
#  trH_within <- 0
#  for (k in 1:number_of_clusters) {
#    if(cardinalita_gruppi[[k]]==1)
#      trHk <- 0
#    else
#      trHk <- (cardinalita_gruppi[[k]] - 1) * sum(agvar[k,])
#    print(paste("Misura di non omogeneità del gruppo", k, ":", trHk))
#    trH_within <- trH_within+trHk
#  }
#
#  trH_between <- trHI - trH_within
#  print(paste("Misura di non omogeneità all’interno dei quattro gruppi (within):", trH_within))
#  print(paste("Misura di non omogeneità tra i cluster (between):", trH_between))
#  print(paste("Rapporto tra la misura di non omogeneità between e quella di non omogeneità statistica totale:", trH_between / trHI))
#}

## Inserimento delle regioni nei cluster
#
#print("Inserimento delle regioni in 4 cluster:")
#sort(cutree(hls , k = 4))
#print("Numerosità dei quattro cluster:")
#table(cutree(hls , k = 4))
#
## Misure di sintesi associate ai cluster
#
#taglio <- list(cutree(hls , k = 4))
#
#print("Media dei quattro cluster:")
#aggregate(df2019, taglio, mean)
#print("Varianza dei quattro cluster:")
#aggregate(df2019, taglio, var)
#print("Deviazione standard dei quattro cluster:")
#aggregate(df2019, taglio, sd)