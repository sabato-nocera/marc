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

# Regressione quadratica

plot(trasporto_pubblico, parcheggio, col ="purple ",main=" Scatterplot")

regressione_quadratica <-lm(parcheggio~trasporto_pubblico+I(trasporto_pubblico^2))
print("Regressione quadratica:")
print(regressione_quadratica)

alpha <- regressione_quadratica$coefficients[[1]]
beta <- regressione_quadratica$coefficients[[2]]
gamma <- regressione_quadratica$coefficients[[3]]

stime <-alpha+beta*trasporto_pubblico+gamma*(trasporto_pubblico)^2
num1 <-sum(((parcheggio-stime)^2) )
den1 <-sum(((parcheggio-mean(parcheggio))^2) )
coefficiente_determinazione_def1 <- 1-(num1/ den1)

print(paste("Coefficiente di determinazione (regressione quadratica): ", coefficiente_determinazione_def1))

regressione_lineare <-lm(parcheggio~trasporto_pubblico)
print(paste("Coefficiente di determinazione (regressione lineare): ", summary(regressione_lineare)$r.square))

par(mfrow=c(1,2))

plot(trasporto_pubblico, parcheggio, col ="purple ",main="Regressione quadratica")
curve(alpha+beta*x+gamma*x^2, add = TRUE, col="red")

plot(trasporto_pubblico, parcheggio, main="Retta di regressione", col ="purple")
abline(regressione_lineare, col ="red")

# Regressione con funzione esponenziale

plot(condizioni_stradali, traffico, col ="purple ",main=" Scatterplot")

regressione_esponenziale <-lm(traffico~I(exp(condizioni_stradali)))
print("Regressione con funzione esponenziale:")
print(regressione_esponenziale)

alpha <- regressione_esponenziale$coefficients[[1]]
beta <- regressione_esponenziale$coefficients[[2]]

stime <- alpha+beta*exp(condizioni_stradali)
num1 <-sum (((traffico-stime)^2))
den1 <-sum (((traffico-mean(traffico))^2))
coefficiente_determinazione_def1 <- 1-(num1/den1)

print(paste("Coefficiente di determinazione (regressione con funzione esponenziale): ", coefficiente_determinazione_def1))

regressione_lineare <-lm(traffico~condizioni_stradali)
print(paste("Coefficiente di determinazione (regressione lineare): ", summary(regressione_lineare)$r.square))

par(mfrow=c(1,2))

plot(condizioni_stradali, traffico, col ="purple ",main="Regressione con funzione esponenziale")
curve(alpha+beta*exp(x), add = TRUE, col="red")

plot(condizioni_stradali, traffico, main="Retta di regressione", col ="purple")
abline(regressione_lineare, col ="red")

# Regressione con trasformazione semilogaritmica

plot(illuminazione_stradale, condizioni_stradali, col = "purple ", main = " Scatterplot")

regressione_semilogaritmica <- lm(I(log(condizioni_stradali)) ~ illuminazione_stradale)
print("Regressione con trasformazione semilogaritmica:")
print(regressione_semilogaritmica)

alpha <- regressione_semilogaritmica$coefficients[[1]]
beta <- regressione_semilogaritmica$coefficients[[2]]

stime <- exp(alpha + beta * illuminazione_stradale)
num1 <- sum(((condizioni_stradali - stime)^2))
den1 <- sum(((condizioni_stradali - mean(condizioni_stradali))^2))
coefficiente_determinazione_def1 <- 1 - (num1 / den1)
print(paste("Coefficiente di determinazione (regressione con trasformazione semilogaritmica): ", coefficiente_determinazione_def1))

regressione_lineare <- lm(condizioni_stradali ~ illuminazione_stradale)
print(paste("Coefficiente di determinazione (regressione lineare): ", summary(regressione_lineare)$r.square))

par(mfrow=c(1,2))

plot(illuminazione_stradale, condizioni_stradali, col = "purple ", main = "Regressione con funzione semilogaritmica")
curve(exp(alpha + beta * x), add = TRUE, col = "red")

plot(illuminazione_stradale, condizioni_stradali, main = "Retta di regressione", col = "purple")
abline(regressione_lineare, col = "red")

# Regressione con trasformazione logaritmica

plot(illuminazione_stradale, condizioni_stradali, col = "purple ", main = " Scatterplot")

regressione_geometrica <- lm(I(log(condizioni_stradali)) ~ I(log(illuminazione_stradale)))
print("Regressione con trasformazione logaritmica:")
print(regressione_geometrica)

alpha <- exp(regressione_geometrica$coefficients[[1]])
beta <- regressione_geometrica$coefficients[[2]]

stime <- alpha*(illuminazione_stradale^beta)
num1 <- sum(((condizioni_stradali - stime)^2))
den1 <- sum(((condizioni_stradali - mean(condizioni_stradali))^2))
coefficiente_determinazione_def1 <- 1 - (num1 / den1)
print(paste("Coefficiente di determinazione (regressione con trasformazione logaritmica): ", coefficiente_determinazione_def1))

regressione_lineare <- lm(condizioni_stradali ~ illuminazione_stradale)
print(paste("Coefficiente di determinazione (regressione lineare): ", summary(regressione_lineare)$r.square))

par(mfrow=c(1,2))

plot(illuminazione_stradale, condizioni_stradali, col = "purple ", main = "Regressione con funzione logaritmica")
curve(alpha*x^beta, add = TRUE, col = "red")

plot(illuminazione_stradale, condizioni_stradali, main = "Retta di regressione", col = "purple")
abline(regressione_lineare, col = "red")