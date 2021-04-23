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

# Metodi di enumerazione completa

# Restituisce il numero di Stirling del secondo tipo
# @param n numero di individui della popolazione
# @param m numero di cluster
stirling2 <- function(n, m) {
  s <- 0
  if ((m >= 1) & (m <= n)) {
    for (k in seq(0, m)) {
      s <- s + (choose(m, k) * (-1)^k * (m - k)^n / factorial(m)) }
    return(c(s))
  }
}

print(paste("Numero di Stirling fissati 2 cluster: ",stirling2(nrow(df2019), 2)))
print(paste("Numero di Stirling fissati 3 cluster: ",stirling2(nrow(df2019), 3)))
print(paste("Numero di Stirling fissati 4 cluster: ",stirling2(nrow(df2019), 4)))
print(paste("Numero di Stirling fissati 5 cluster: ",stirling2(nrow(df2019), 5)))

# Restituisce il numero di Bell n-esimo
# @param n numero di individui della popolazione
sumstirling2 <- function(n) {
  s <- 0
  for (k in seq(1, n))
    s <- s + stirling2(n, k)
  return(c(s))
}

print(paste("Numero di Bell B(22): ", sumstirling2(nrow(df2019))))