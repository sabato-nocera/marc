# Configurazione dell’ambiente di sviluppo
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

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

# Regressione lineare multipla

print("Matrice delle covarianze campionarie:")
cov(df2019)
print("Matrice delle correlazioni:")
cor(df2019)

print(paste("Coefficiente di determinazione dell'illuminazione stradale in funzione delle condizioni stradali: ",
            summary(lm(illuminazione_stradale~condizioni_stradali))$r.square))
print(paste("Coefficiente di determinazione dell'illuminazione stradale in funzione delle condizioni stradali e del trasporto pubblico: ",
            summary(lm(illuminazione_stradale~condizioni_stradali+trasporto_pubblico))$r.square))
print(paste("Coefficiente di determinazione dell'illuminazione stradale in funzione delle condizioni stradali, del trasporto pubblico e del parcheggio: ",
            summary(lm(illuminazione_stradale~condizioni_stradali+trasporto_pubblico+parcheggio))$r.square))
print(paste("Coefficiente di determinazione dell'illuminazione stradale in funzione delle condizioni stradali, del trasporto pubblico, del parcheggio e del traffico: ",
            summary(lm(illuminazione_stradale~condizioni_stradali+trasporto_pubblico+parcheggio+traffico))$r.square))

linear_model <- lm(illuminazione_stradale~condizioni_stradali+trasporto_pubblico+parcheggio)
print("Linear model:")
print(linear_model)

vettore_valori_stimati <- fitted(linear_model)
print("Vettore dei valori stimati:")
print(vettore_valori_stimati)

vettore_residui <- resid(linear_model)
print("Vettore dei residui:")
print(vettore_residui)

print(paste("Mediana dei residui:", median(vettore_residui)))
print(paste("Varianza dei residui:", var(vettore_residui)))
print(paste("Deviazione standard dei residui:", sd(vettore_residui)))
print(paste("Media campionaria:", mean(vettore_residui)))

vettore_residui_standard <- vettore_residui/sd(vettore_residui)
print("Vettore dei residui standard:")
print(vettore_residui_standard)

plot(vettore_valori_stimati, vettore_residui_standard , main="Residui standard rispetto ai valori stimati",
     xlab="Valori stimati", ylab="Residui standard",pch=5, col ="purple")
abline (h=0, col =" red",lty =2)

