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

linear_model_1 <- lm(traffico~parcheggio)
print("Linear model (parcheggio,traffico):")
print(linear_model_1)

vettore_valori_stimati_1 <- fitted(linear_model_1)
print("Vettore dei valori stimati:")
print(vettore_valori_stimati_1)

vettore_residui_1 <- resid(linear_model_1)
print("Vettore dei residui:")
print(vettore_residui_1)

print(paste("Mediana dei residui:", median(vettore_residui_1)))
print(paste("Varianza dei residui:", var(vettore_residui_1)))
print(paste("Deviazione standard dei residui:", sd(vettore_residui_1)))
print(paste("Media campionaria:", mean(vettore_residui_1)))

plot(parcheggio, traffico, main="Retta di regressione e residui", xlab = rownames(parcheggio), ylab = rownames(traffico), col ="purple")
abline(lm(traffico~parcheggio), col ="red")
segments(parcheggio, vettore_valori_stimati_1, parcheggio, traffico, col ="orange2")

plot(parcheggio, vettore_residui_1 , main="Diagramma dei residui", xlab=rownames(parcheggio), ylab="residui ", pch=9, col ="purple")
abline(h=0, col="red ", lty=2)

vettore_residui_standard_1 <- vettore_residui_1/sd(vettore_residui_1)
print("Vettore dei residui standard:")
print(vettore_residui_standard_1)
plot(vettore_valori_stimati_1, vettore_residui_standard_1, main =" Residui standard rispetto ai valori stimati", xlab="valori stimati", ylab ="residui standard", pch=5, col ="purple")
abline (h=0, col="red", lty=2)

print(paste("Coefficiente di determinazione: ", (cor(parcheggio,traffico))^2))

# Illuminazione stradale e condizioni stradali

linear_model_2 <- lm(condizioni_stradali~illuminazione_stradale)
print("Linear model (illuminazione_stradale, condizioni_stradali):")
print(linear_model_2)

vettore_valori_stimati_2 <- fitted(linear_model_2)
print("Vettore dei valori stimati:")
print(vettore_valori_stimati_2)

vettore_residui_2 <- resid(linear_model_2)
print("Vettore dei residui:")
print(vettore_residui_2)

print(paste("Mediana dei residui:", median(vettore_residui_2)))
print(paste("Varianza dei residui:", var(vettore_residui_2)))
print(paste("Deviazione standard dei residui:", sd(vettore_residui_2)))
print(paste("Media campionaria:", mean(vettore_residui_2)))

plot(illuminazione_stradale, condizioni_stradali, main="Retta di regressione e residui", xlab = rownames(illuminazione_stradale), ylab = rownames(condizioni_stradali), col ="purple")
abline(lm(condizioni_stradali~illuminazione_stradale), col ="red")
segments(illuminazione_stradale, vettore_valori_stimati_2, illuminazione_stradale, condizioni_stradali, col ="orange2")

plot(illuminazione_stradale, vettore_residui_2 , main="Diagramma dei residui", xlab=rownames(illuminazione_stradale), ylab="residui ", pch=9, col ="purple")
abline(h=0, col="red ", lty=2)

vettore_residui_standard_2 <- vettore_residui_2/sd(vettore_residui_2)
print("Vettore dei residui standard:")
print(vettore_residui_standard_2)
plot(vettore_valori_stimati_2, vettore_residui_standard_2, main =" Residui standard rispetto ai valori stimati", xlab="valori stimati", ylab ="residui standard", pch=5, col ="purple")
abline (h=0, col="red", lty=2)

print(paste("Coefficiente di determinazione: ", (cor(illuminazione_stradale,condizioni_stradali))^2))