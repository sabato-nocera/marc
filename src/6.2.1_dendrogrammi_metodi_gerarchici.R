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

#write.csv(round(hls$height,3), "file.csv", row.names = FALSE)

# Screeplot

df_scaled <- scale(df2019)

#Legame singolo

matrice_distanze <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)

hls <- hclust(matrice_distanze, method = "single")

str(hls)

plot(c(0, hls$height), seq(22,1), type="b", main="Screeplot metodo del legame semplice", xlab="Distanza di aggregazione", ylab="Numero di cluster", axes=FALSE, col ="red")
axis(side=1, at=round(c(0, hls$height),2))
axis(side=2, at=seq(22,1), las=2)

#Legame completo

matrice_distanze <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)

hls <- hclust(matrice_distanze, method = "complete")

plot(c(0, hls$height), seq(22,1), type="b", main="Screeplot metodo del legame completo", xlab="Distanza di aggregazione", ylab="Numero di cluster", axes=FALSE, col ="red")
axis(side=1, at=round(c(0, hls$height),2))
axis(side=2, at=seq(22,1), las=2)

#Legame medio

matrice_distanze <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)

hls <- hclust(matrice_distanze, method = "average")

plot(c(0, hls$height), seq(22,1), type="b", main="Screeplot metodo del legame medio", xlab="Distanza di aggregazione", ylab="Numero di cluster", axes=FALSE, col ="red")
axis(side=1, at=round(c(0, hls$height),2))
axis(side=2, at=seq(22,1), las=2)

#Centroide

matrice_distanze2 <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)^2

hls <- hclust(matrice_distanze2, method = "centroid")

plot(c(0, hls$height), seq(22,1), type="b", main="Screeplot metodo del centroide", xlab="Distanza di aggregazione", ylab="Numero di cluster", axes=FALSE, col ="red")
axis(side=1, at=round(c(0, hls$height),2))
axis(side=2, at=seq(22,1), las=2)

#Mediana

matrice_distanze2 <- dist(df_scaled, method ="euclidean", diag=TRUE ,upper = TRUE)^2

hls <- hclust(matrice_distanze2, method = "median")

str(hls)

plot(c(0, hls$height), seq(22,1), type="b", main="Screeplot metodo della mediana", xlab="Distanza di aggregazione", ylab="Numero di cluster", axes=FALSE, col ="red")
axis(side=1, at=round(c(0, hls$height),2))
axis(side=2, at=seq(22,1), las=2)


