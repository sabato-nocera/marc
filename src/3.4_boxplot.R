# Configurazione dell’ambiente di sviluppo
df2019 <- read.csv("dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

regioni <- rownames(df2019)

# Quartili 2019

parcheggio <- df2019[[1]]
trasporto_pubblico <- df2019[[2]]
traffico <- df2019[[3]]
illuminazione_stradale <- df2019[[4]]
condizioni_stradali <- df2019[[5]]

colori_caldi <- heat.colors(5)

print("Quartili relativi allla difficoltà nel trovare un parcheggio:")
quantile(parcheggio)
summary(quantile(parcheggio))
boxplot(parcheggio , col = colori_caldi[1], horizontal = TRUE, main="Difficoltà nel trovare un parcheggio", xlab="Famiglie che hanno espressione un giudizio molto negativo")

print("Quartili relativi alla difficoltà di collegamento con mezzi pubblici:")
quantile(trasporto_pubblico)
summary(quantile(trasporto_pubblico))
boxplot(trasporto_pubblico , col = colori_caldi[2], horizontal = TRUE, main="Difficoltà di collegamento con mezzi pubblici", xlab="Famiglie che hanno espressione un giudizio molto negativo")

print("Quartili relativi al traffico:")
quantile(traffico)
summary(quantile(traffico))
boxplot(traffico , col = colori_caldi[3], horizontal = TRUE, main="Traffico", xlab="Famiglie che hanno espressione un giudizio molto negativo")

print("Quartili relativi alla scarsa illuminazione stradale:")
quantile(illuminazione_stradale)
summary(quantile(illuminazione_stradale))
boxplot(illuminazione_stradale , col = colori_caldi[4], horizontal = TRUE, main="Scarsa illuminazione stradale", xlab="Famiglie che hanno espressione un giudizio molto negativo")

print("Quartili relativi alle cattive condizioni stradali:")
quantile(condizioni_stradali)
summary(quantile(condizioni_stradali))
boxplot(condizioni_stradali , col = colori_caldi[5], horizontal = TRUE, main="Cattive condizioni stradali", xlab="Famiglie che hanno espressione un giudizio molto negativo")


