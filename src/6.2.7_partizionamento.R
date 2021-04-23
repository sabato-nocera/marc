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

#

df_scaled <- scale(df2019)

matrice_distanze <- dist(df_scaled, method = "euclidean", diag = TRUE, upper = TRUE)
matrice_distanze2 <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)^2

par(mfrow=c(1,2))

plot(hclust(matrice_distanze, method = "complete"), hang =-1, axes=FALSE, xlab=" Metodo gerarchico agglomerativo del legame completo", sub ="con 4 cluster")
axis(side=2, at=round(c(0, (hclust(matrice_distanze, method = "complete"))$height),2), las=2)
rect.hclust(hclust(matrice_distanze, method = "complete"), k = 4, border = "orange")

plot(hclust(matrice_distanze2, method = "median"), hang =-1, axes=FALSE, xlab=" Metodo gerarchico agglomerativo della mediana", sub ="con 4 cluster")
axis(side=2, at=round(c(0, (hclust(matrice_distanze2, method = "median"))$height),2), las=2)
rect.hclust(hclust(matrice_distanze2, method = "median"), k = 4, border = "orange")

