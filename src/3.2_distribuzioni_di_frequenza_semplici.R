# Configurazione dell’ambiente di sviluppo
df2018 <- read.csv("../../dataset/2018 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2018) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")
colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

df2018
df2019

str(df2018)
str(df2019)

regioni <- rownames(df2019)

# Tabelle di frequenza

lunghezza_sequenza <- 6

# Parcheggio

par(mfrow=c(2,2))
parcheggio <- df2019[[1]]
names(parcheggio) <- regioni
minimo <- floor(min(parcheggio))
massimo <- ceiling(max(parcheggio))
classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)

print("Distribuzione di frequenza assoluta circa la difficoltà nel trovare parcheggio:")
table(cut(parcheggio, classi))
barplot(table(cut(parcheggio, classi)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza assoluta")

print("Distribuzione di frequenza relativa circa la difficoltà nel trovare parcheggio:")
table(cut(parcheggio, classi))/length(parcheggio)
barplot(table(cut(parcheggio, classi))/length(parcheggio), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza relativa")

print("Frequenze assolute cumulate circa la difficoltà nel trovare parcheggio:")
cumsum(table(cut(parcheggio, classi)))
barplot(cumsum(table(cut(parcheggio, classi))), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze assolute cumulate")

print("Frequenze relative cumulate circa la difficoltà nel trovare parcheggio:")
cumsum(table(cut(parcheggio, classi))/length(parcheggio))
barplot(cumsum(table(cut(parcheggio, classi))/length(parcheggio)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze relative cumulate")

mtext("Difficoltà nel trovare parcheggio", side = 3, line = -24, outer = TRUE, col="orange", cex=2)

# Trasporto pubblico

par(mfrow=c(2,2))
trasporto_pubblico <- df2019[[2]]
names(trasporto_pubblico) <- regioni
minimo <- floor(min(trasporto_pubblico))
massimo <- ceiling(max(trasporto_pubblico))
classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)

print("Distribuzione di frequenza assoluta circa la difficoltà di collegamento con mezzi pubblici:")
table(cut(trasporto_pubblico, classi))
barplot(table(cut(trasporto_pubblico, classi)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza assoluta")

print("Distribuzione di frequenza relativa circa la difficoltà di collegamento con mezzi pubblici:")
table(cut(trasporto_pubblico, classi))/length(trasporto_pubblico)
barplot(table(cut(trasporto_pubblico, classi))/length(trasporto_pubblico), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza relativa")

print("Frequenze assolute cumulate circa la difficoltà di collegamento con mezzi pubblici:")
cumsum(table(cut(trasporto_pubblico, classi)))
barplot(cumsum(table(cut(trasporto_pubblico, classi))), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze assolute cumulate")

print("Frequenze relative cumulate circa la difficoltà di collegamento con mezzi pubblici:")
cumsum(table(cut(trasporto_pubblico, classi))/length(trasporto_pubblico))
barplot(cumsum(table(cut(trasporto_pubblico, classi))/length(trasporto_pubblico)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze relative cumulate")

mtext("Difficoltà di collegamento con mezzi pubblici", side = 3, line = -24, outer = TRUE, col="orange", cex=2)


# Traffico

par(mfrow=c(2,2))
traffico <- df2019[[3]]
names(traffico) <- regioni
minimo <- floor(min(traffico))
massimo <- ceiling(max(traffico))
classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)

print("Distribuzione di frequenza assoluta circa il traffico:")
table(cut(traffico, classi))
barplot(table(cut(traffico, classi)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza assoluta")

print("Distribuzione di frequenza relativa circa il traffico:")
table(cut(traffico, classi))/length(traffico)
barplot(table(cut(traffico, classi))/length(traffico), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza relativa")

print("Frequenze assolute cumulate circa il traffico:")
cumsum(table(cut(traffico, classi)))
barplot(cumsum(table(cut(traffico, classi))), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze assolute cumulate")

print("Frequenze relative cumulate circa il traffico:")
cumsum(table(cut(traffico, classi))/length(traffico))
barplot(cumsum(table(cut(traffico, classi))/length(traffico)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze relative cumulate")

mtext("Traffico", side = 3, line = -21, outer = TRUE, col="orange", cex=2)

# Illuminazione stradale

par(mfrow=c(2,2))
illuminazione_stradale <- df2019[[4]]
names(illuminazione_stradale) <- regioni
minimo <- floor(min(illuminazione_stradale))
massimo <- ceiling(max(illuminazione_stradale))
classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)

print("Distribuzione di frequenza assoluta circa la scarsa illuminazione stradale:")
table(cut(illuminazione_stradale, classi))
barplot(table(cut(illuminazione_stradale, classi)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza assoluta")

print("Distribuzione di frequenza relativa circa la scarsa illuminazione stradale:")
table(cut(illuminazione_stradale, classi))/length(illuminazione_stradale)
barplot(table(cut(illuminazione_stradale, classi))/length(illuminazione_stradale), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza relativa")

print("Frequenze assolute cumulate circa la scarsa illuminazione stradale:")
cumsum(table(cut(illuminazione_stradale, classi)))
barplot(cumsum(table(cut(illuminazione_stradale, classi))), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze assolute cumulate")

print("Frequenze relative cumulate circa la scarsa illuminazione stradale:")
cumsum(table(cut(illuminazione_stradale, classi))/length(illuminazione_stradale))
barplot(cumsum(table(cut(illuminazione_stradale, classi))/length(illuminazione_stradale)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze relative cumulate")

mtext("Scarsa illuminazione stradale", side = 3, line = -21, outer = TRUE, col="orange", cex=2)

# Condizioni stradali

par(mfrow=c(2,2))
condizioni_stradali <- df2019[[5]]
names(condizioni_stradali) <- regioni
minimo <- floor(min(condizioni_stradali))
massimo <- ceiling(max(condizioni_stradali))
classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)

print("Distribuzione di frequenza assoluta circa le cattive condizioni delle strade:")
table(cut(condizioni_stradali, classi))
barplot(table(cut(condizioni_stradali, classi)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza assoluta")

print("Distribuzione di frequenza relativa circa le cattive condizioni delle strade:")
table(cut(condizioni_stradali, classi))/length(condizioni_stradali)
barplot(table(cut(condizioni_stradali, classi))/length(condizioni_stradali), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Distribuzione di frequenza relativa")

print("Frequenze assolute cumulate circa le cattive condizioni delle strade:")
cumsum(table(cut(condizioni_stradali, classi)))
barplot(cumsum(table(cut(condizioni_stradali, classi))), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze assolute cumulate")

print("Frequenze relative cumulate circa le cattive condizioni delle strade:")
cumsum(table(cut(condizioni_stradali, classi))/length(condizioni_stradali))
barplot(cumsum(table(cut(condizioni_stradali, classi))/length(condizioni_stradali)), cex.names = 0.9, col=rainbow(lunghezza_sequenza-1), main = "Frequenze relative cumulate")

mtext("Cattive condizioni delle strade", side = 3, line = -21, outer = TRUE, col="orange", cex=2)