# Configurazione dell’ambiente di sviluppo
df2018 <- read.csv("dataset/2018 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)
df2019 <- read.csv("dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2018) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")
colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

df2018
df2019

str(df2018)
str(df2019)

regioni <- rownames(df2019)

# Rappresentazione grafica delle colonne del dataset

parcheggio <- df2019[[1]]
names(parcheggio) <- regioni
barplot(parcheggio, mgp=c(3,0.55,-0.5), sub = "(su 100 famiglie con le stesse caratteristiche)\n\n", main = "Difficoltà nel trovare un parcheggio", space = 0, cex.names = 0.7, horiz = T, las=2, col=rainbow(length(regioni)))

trasporto_pubblico <- df2019[[2]]
names(trasporto_pubblico) <- regioni
barplot(trasporto_pubblico, sub = "(su 100 famiglie con le stesse caratteristiche)\n\n", main = "Difficoltà di collegamento con mezzi pubblici", mgp=c(3,0.55,-0.5), space = 0, cex.names = 0.7, horiz = T, las=2, col=rainbow(length(regioni)))

traffico <- df2019[[3]]
names(traffico) <- regioni
barplot(traffico, main = "Traffico", sub = "(su 100 famiglie con le stesse caratteristiche)\n\n", mgp=c(3,0.55,-0.5), space = 0, cex.names = 0.7, horiz = T, las=2, col=rainbow(length(regioni)))

illuminazione_stradale <- df2019[[4]]
names(illuminazione_stradale) <- regioni
barplot(illuminazione_stradale, sub = "(su 100 famiglie con le stesse caratteristiche)\n\n", main = "Scarsa illuminazione stradale", mgp=c(3,0.55,-0.5), space = 0, cex.names = 0.7, horiz = T, las=2, col=rainbow(length(regioni)))

condizioni_stradali <- df2019[[5]]
names(condizioni_stradali) <- regioni
barplot(condizioni_stradali, sub = "(su 100 famiglie con le stesse caratteristiche)\n\n", main = "Cattive condizioni stradali", mgp=c(3,0.55,-0.5), space = 0, cex.names = 0.7, horiz = T, las=2, col=rainbow(length(regioni)))

# Diagrammi di dispersione (scatterplot)

pairs(df2019, cex.labels = 2.5, main=" Scatterplot  per  le  coppie  di  caratteristiche")