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

# Indici di sintesi o statistiche

curt <- function(x) {
  n <- length(x)
  m2 <- (n - 1) * var(x) / n
  m4 <- (sum((x - mean(x))^4)) / n
  m4 / (m2^2) - 3
}

skw <- function(x) {
  n <- length(x)
  m2 <- (n - 1) * var(x) / n
  m3 <- (sum((x - mean(x))^3)) / n
  m3 / (m2^1.5)
}

tipi_quartili <- function(x, main_text) {
  y <- numeric(0)
  for (i in 1:9) {
    y <- rbind(y, c(quantile(x, 0, type = i), quantile(x, 0.25, type = i),
                    quantile(x, 0.5, type = i), quantile(x, 0.75, type = i),
                    quantile(x, 1, type = i))) }
  rownames(y) <- paste("type", 1:9)
  print(paste("Tipi di quartili", main_text))
  print(y)
}

mediana_di_distribuzione_empirica_continua <- function(vettore, main_text, lunghezza_sequenza = 6) {
  par(mfrow = c(1, 1))
  minimo <- floor(min(vettore))
  massimo <- ceiling(max(vettore))
  classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza), 0)
  table <- table(cut(vettore, classi, right = FALSE))
  prop_table <- prop.table(table)
  frequenze_relative_cumulative <- cumsum(prop_table)
  print(paste("Frequenze relativa cumulative", main_text))
  print(frequenze_relative_cumulative)
  ascisse <- c(min(classi) - 4, classi, max(classi) + 4)
  ordinate <- c(0, 0, frequenze_relative_cumulative, 1)

  plot(ascisse, ordinate, type = "b", font.lab = 2, axes = FALSE, main = paste("Funzione di distribuzione empirica continua\n", main_text), col = "purple", ylim = c(0, 1), xlab = "x", ylab = "F(x)")
  abline(h = 0.5, lty = 2, col = "red")

  axis(1, ascisse)
  axis(2, format(c(0, frequenze_relative_cumulative), digits = 2), las = 2)
  box()
}

indici_di_sintesi <- function(vettore, main_text, lunghezza_sequenza = 6) {
  print(paste("Indici di sintesi relativi a", main_text))

  # Misure di centralità: media, mediana, moda campionarie
  media <- mean(vettore)
  print(paste("Media", main_text, ":", media))
  mediana <- median(vettore)
  print(paste("Mediana", main_text, ":", mediana))
  minimo <- floor(min(vettore))
  massimo <- ceiling(max(vettore))
  classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza), 0)
  frequenze_assolute_ordinate <- sort(table(cut(vettore, classi)), decreasing = TRUE)
  classe_modale <- rownames(frequenze_assolute_ordinate)[1]
  print(paste("Classe modale", main_text, ":", classe_modale))

  hist(vettore, breaks = classi, axes = FALSE, freq = TRUE, font.lab = 2, main = paste("Istogramma delle frequenze assolute\n", main_text), col = heat.colors(lunghezza_sequenza - 1), xlab = "", ylab = "")
  axis(1, classi)
  axis(2, c(0, frequenze_assolute_ordinate), las = 2)

  # Mediana per una distribuzione di frequenze

  mediana_distribuzione_di_frequenza <- quantile(vettore, 0.5, type = 1)

  print(paste("Mediana della distribuzione di frequenza", main_text, ":", mediana_distribuzione_di_frequenza))

  mediana_di_distribuzione_empirica_continua(vettore, main_text)

  # Quartili

  tipi_quartili(vettore, main_text)

  # Dipsersione dei dati: Varianza, deviazione standard e coefficiente di variazione

  print(paste("Varianza", main_text, ":", var(vettore)))
  print(paste("Deviazione standard", main_text, ":", sd(vettore)))
  print(paste("Coefficiente di variazione", main_text, ":", sd(vettore) / abs(mean(vettore))))

  # Forma di una distribuzione di frequenze

  print(paste("Skewness (asimmetria)", main_text, ":", skw(vettore)))
  print(paste("Curtosi campionaria", main_text, ":", curt(vettore)))

}

indici_di_sintesi(parcheggio, "per la difficoltà nel trovare parcheggio")
#indici_di_sintesi(trasporto_pubblico, "per la difficoltà di collegamento con mezzi pubblici")
#indici_di_sintesi(traffico, "per il traffico")
#indici_di_sintesi(illuminazione_stradale, "per la scarsa illuminazione stradale")
#indici_di_sintesi(condizioni_stradali, "per le cattive condizioni delle strade")