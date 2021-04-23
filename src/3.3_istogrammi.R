# Configurazione dell’ambiente di sviluppo
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

regioni <- rownames(df2019)

# Istogrammi

parcheggio <- df2019[[1]]
trasporto_pubblico <- df2019[[2]]
traffico <- df2019[[3]]
illuminazione_stradale <- df2019[[4]]
condizioni_stradali <- df2019[[5]]

par(mfrow=c(3,2))

plot.new()
istogramma_parcheggio <- hist(parcheggio, cex.axis=1.2, cex.lab=1.5, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Frequenza assoluta delle classi", main = "Parcheggio", col = rainbow(5))
text(istogramma_parcheggio$mids,istogramma_parcheggio$counts,labels=istogramma_parcheggio$counts, adj=c(0.5, -0.2))
istogramma_trasporto_pubblico <- hist(trasporto_pubblico, cex.axis=1.2, cex.lab=1.5, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Frequenza assoluta delle classi", main = "Trasporto pubblico", col = rainbow(4))
text(istogramma_trasporto_pubblico$mids,istogramma_trasporto_pubblico$counts,labels=istogramma_trasporto_pubblico$counts, adj=c(0.5, -0.2))
istogramma_traffico <- hist(traffico, cex.axis=1.2, cex.lab=1.5, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Frequenza assoluta delle classi", main = "Traffico", col = rainbow(8))
text(istogramma_traffico$mids,istogramma_traffico$counts,labels=istogramma_traffico$counts, adj=c(0.5, -0.2))
istogramma_illuminazione_stradale <- hist(illuminazione_stradale, cex.axis=1.2, cex.lab=1.5, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Frequenza assoluta delle classi", main = "Illuminazione stradale", col = rainbow(6))
text(istogramma_illuminazione_stradale$mids,istogramma_illuminazione_stradale$counts,labels=istogramma_illuminazione_stradale$counts, adj=c(0.5, -0.2))
istogramma_condizioni_stradali <- hist(condizioni_stradali, cex.axis=1.2, cex.lab=1.5, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Frequenza assoluta delle classi", main = "Condizioni stradali", col = rainbow(7))
text(istogramma_condizioni_stradali$mids,istogramma_condizioni_stradali$counts,labels=istogramma_condizioni_stradali$counts, adj=c(0.5, -0.2))
mtext("Istogrammi delle\nfrequenze assolute\ndelle classi", cex=3, side = 3, line = -17, at = 0.25, outer = TRUE, col="turquoise4")

plot.new()
istogramma_parcheggio <- hist(parcheggio, cex.axis=1.0, cex.lab=1.5, freq = FALSE, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Densità di frequenza delle classi", main = "Parcheggio", col = rainbow(5))
istogramma_trasporto_pubblico <- hist(trasporto_pubblico, cex.axis=1.0, cex.lab=1.5, freq = FALSE, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Densità di frequenza delle classi", main = "Trasporto pubblico", col = rainbow(4))
istogramma_traffico <- hist(traffico, cex.axis=1.0, cex.lab=1.5, freq = FALSE, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Densità di frequenza delle classi", main = "Traffico", col = rainbow(8))
istogramma_illuminazione_stradale <- hist(illuminazione_stradale, cex.axis=1.0, cex.lab=1.5, freq = FALSE, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Densità di frequenza delle classi", main = "Illuminazione stradale", col = rainbow(6))
istogramma_condizioni_stradali <- hist(condizioni_stradali, cex.axis=1.0, cex.lab=1.5, freq = FALSE, xlab="Famiglie che hanno espressio un giudizio molto negativo", ylab = "Densità di frequenza delle classi", main = "Condizioni stradali", col = rainbow(7))
mtext("Istogrammi delle\ndensità di frequenza\ndelle classi", cex=3, side = 3, line = -17, at = 0.25, outer = TRUE, col="turquoise4")

print("Informazioni relative all'istogramma del parcheggio:")
str(istogramma_parcheggio)
print("Informazioni relative all'istogramma del trasporto pubblico:")
str(istogramma_trasporto_pubblico)
print("Informazioni relative all'istogramma del traffico:")
str(istogramma_traffico)
print("Informazioni relative all'istogramma dell'illuminazione stradale:")
str(istogramma_illuminazione_stradale)
print("Informazioni relative all'istogramma delle condizioni stradali:")
str(istogramma_condizioni_stradali)