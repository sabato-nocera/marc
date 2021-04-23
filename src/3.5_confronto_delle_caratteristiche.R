# Configurazione dell’ambiente di sviluppo
df2018 <- read.csv("../../dataset/2018 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2018) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")
colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

regioni <- rownames(df2019)

#  2019

parcheggio_2019 <- df2019[[1]]
trasporto_pubblico_2019 <- df2019[[2]]
traffico_2019 <- df2019[[3]]
illuminazione_stradale_2019 <- df2019[[4]]
condizioni_stradali_2019 <- df2019[[5]]

colori_caldi <- heat.colors(5)

colore_2019 <- "orangered4"

# 2018

parcheggio_2018 <- df2018[[1]]
trasporto_pubblico_2018 <- df2018[[2]]
traffico_2018 <- df2018[[3]]
illuminazione_stradale_2018 <- df2018[[4]]
condizioni_stradali_2018 <- df2018[[5]]

colori_freddi <- cm.colors(5)

colore_2018 <- "dark blue"

# Plot e points

par(mfrow=c(1,1))

plot(parcheggio_2019, pch = "+", xlab = "Indice delle regioni", ylab = "Famiglie che hanno espresso un giudizio molto negativo", main = "Difficoltà nel trovare un parcheggio", col = colore_2019)
points (parcheggio_2018, pch = "x", col = colore_2018)
legend("topright", c("2019","2018"), pch =c("+","x"), col =c(colore_2019,colore_2018), inset=.02,  box.lty=2, box.lwd=2, box.col="seagreen4", bg = "palegreen", text.col = "black", cex = 1.2)

plot(trasporto_pubblico_2019, pch = "+", xlab = "Indice delle regioni", ylab = "Famiglie che hanno espresso un giudizio molto negativo", main = "Difficoltà di collegamento con mezzi pubblici", col = colore_2019)
points (trasporto_pubblico_2018, pch = "x", col = colore_2018)
legend("topright", c("2019","2018"), pch =c("+","x"), col =c(colore_2019,colore_2018), inset=.02,  box.lty=2, box.lwd=2, box.col="seagreen4", bg = "palegreen", text.col = "black", cex = 1.2)

plot(traffico_2019, pch = "+", xlab = "Indice delle regioni", ylab = "Famiglie che hanno espresso un giudizio molto negativo", main = "Traffico", col = colore_2019)
points (traffico_2018, pch = "x", col = colore_2018)
legend("topright", c("2019","2018"), pch =c("+","x"), col =c(colore_2019,colore_2018), inset=.02,  box.lty=2, box.lwd=2, box.col="seagreen4", bg = "palegreen", text.col = "black", cex = 1.2)

plot(illuminazione_stradale_2019, pch = "+", xlab = "Indice delle regioni", ylab = "Famiglie che hanno espresso un giudizio molto negativo", main = "Scarsa illuminazione stradale", col = colore_2019)
points (illuminazione_stradale_2018, pch = "x", col = colore_2018)
legend("topleft", c("2019","2018"), pch =c("+","x"), col =c(colore_2019,colore_2018), inset=.02,  box.lty=2, box.lwd=2, box.col="seagreen4", bg = "palegreen", text.col = "black", cex = 1.2)

plot(condizioni_stradali_2019, pch = "+", xlab = "Indice delle regioni", ylab = "Famiglie che hanno espresso un giudizio molto negativo", main = "Cattive condizioni stradali", col = colore_2019)
points (condizioni_stradali_2018, pch = "x", col = colore_2018)
legend("topright", c("2019","2018"), pch =c("+","x"), col =c(colore_2019,colore_2018), inset=.02,  box.lty=2, box.lwd=2, box.col="seagreen4", bg = "palegreen", text.col = "black", cex = 1.2)

# Boxplot ad intaglio

par(mfrow=c(1,2))

boxplot(parcheggio_2019 ,parcheggio_2018 , notch = TRUE, names =c("2019","2018"), col=c(colori_caldi[1],colori_freddi[1]), main="Difficoltà nel trovare un parcheggio")
boxplot(parcheggio_2019 ,parcheggio_2018 ,names =c("2019","2018"), col=c(colori_caldi[1],colori_freddi[1]), main="Difficoltà nel trovare un parcheggio")
print("Quantili relativi alla difficoltà nel trovare un parcheggio")
print("2019")
quantile(parcheggio_2019)
summary(quantile(parcheggio_2019))
print("2018")
quantile(parcheggio_2018)
summary(quantile(parcheggio_2018))

boxplot(trasporto_pubblico_2019 , notch = TRUE, trasporto_pubblico_2018 ,names =c("2019","2018"), col=c(colori_caldi[2],colori_freddi[2]), main="Difficoltà di collegamento con mezzi pubblici")
boxplot(trasporto_pubblico_2019 ,trasporto_pubblico_2018 ,names =c("2019","2018"), col=c(colori_caldi[2],colori_freddi[2]), main="Difficoltà di collegamento con mezzi pubblici")
print("Quantili relativi alla difficoltà di collegamento con mezzi pubblici")
print("2019")
quantile(trasporto_pubblico_2019)
summary(quantile(trasporto_pubblico_2019))
print("2018")
quantile(trasporto_pubblico_2018)
summary(quantile(trasporto_pubblico_2018))

boxplot(traffico_2019 ,traffico_2018 , notch = TRUE, names =c("2019","2018"), col=c(colori_caldi[3],colori_freddi[3]), main="Traffico")
boxplot(traffico_2019 ,traffico_2018 ,names =c("2019","2018"), col=c(colori_caldi[3],colori_freddi[3]), main="Traffico")
print("Quantili relativi al traffico")
print("2019")
quantile(traffico_2019)
summary(quantile(traffico_2019))
print("2018")
quantile(traffico_2018)
summary(quantile(traffico_2018))

boxplot(illuminazione_stradale_2019 ,illuminazione_stradale_2018 , notch = TRUE, names =c("2019","2018"), col=c(colori_caldi[4],colori_freddi[4]), main="Scarsa illuminazione stradale")
boxplot(illuminazione_stradale_2019 ,illuminazione_stradale_2018 ,names =c("2019","2018"), col=c(colori_caldi[4],colori_freddi[4]), main="Scarsa illuminazione stradale")
print("Quantili relativi alla scarsa illuminazione stradale")
print("2019")
quantile(illuminazione_stradale_2019)
summary(quantile(illuminazione_stradale_2019))
print("2018")
quantile(illuminazione_stradale_2018)
summary(quantile(illuminazione_stradale_2018))

boxplot(condizioni_stradali_2019 ,condizioni_stradali_2018 , notch = TRUE, names =c("2019","2018"), col=c(colori_caldi[5],colori_freddi[5]), main="Cattive condizioni stradali")
boxplot(condizioni_stradali_2019 ,condizioni_stradali_2018 ,names =c("2019","2018"), col=c(colori_caldi[5],colori_freddi[5]), main="Cattive condizioni stradali")
print("Quantili relativi alle cattive condizioni stradali")
print("2019")
quantile(condizioni_stradali_2019)
summary(quantile(condizioni_stradali_2019))
print("2018")
quantile(condizioni_stradali_2018)
summary(quantile(condizioni_stradali_2018))
