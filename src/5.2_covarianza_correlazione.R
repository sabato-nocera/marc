# Configurazione dell’ambiente di sviluppo
df2018 <- read.csv("../../dataset/2018 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2018) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")
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

# Parcheggio e traffico

print(paste("Covarianza campionaria (parcheggio,traffico): ", cov(parcheggio,traffico)))
print(paste("Coefficiente di correlazione campionario (parcheggio,traffico): ", cor(parcheggio,traffico)))
plot(parcheggio, traffico, main="Retta di regressione", xlab = rownames(parcheggio), ylab = rownames(traffico), col="purple")
abline(lm(traffico~parcheggio), col ="red")

# Illuminazione stradale e condizioni stradali

print(paste("Covarianza campionaria (illuminazione_stradale, condizioni_stradali): ", cov(illuminazione_stradale, condizioni_stradali)))
print(paste("Coefficiente di correlazione campionario (illuminazione_stradale, condizioni_stradali): ", cor(illuminazione_stradale, condizioni_stradali)))
plot(illuminazione_stradale, condizioni_stradali, main="Retta di regressione", xlab = rownames(illuminazione_stradale), ylab = rownames(condizioni_stradali), col="purple")
abline(lm(condizioni_stradali~illuminazione_stradale), col ="red")