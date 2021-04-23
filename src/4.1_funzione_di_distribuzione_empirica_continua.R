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

#

funzione_di_distribuzione_empirica_continua <- function(vettore, main_text, lunghezza_sequenza = 6 ) {
  par(mfrow=c(1,1))
  minimo <- floor(min(vettore))
  massimo <- ceiling(max(vettore))
  classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)
  table <- table(cut(vettore, classi, right = FALSE))
  prop_table <- prop.table(table)
  frequenze_relative_cumulative <- cumsum(prop_table)
  print(paste("Frequenze relativa cumulative", main_text))
  print(frequenze_relative_cumulative)
  ascisse <-c(min(classi)-4, classi, max(classi)+4)
  ordinate <-c(0, 0, frequenze_relative_cumulative, 1)

  plot(ascisse , ordinate , type = "b", font.lab = 2, axes = FALSE , main = paste("Funzione di distribuzione empirica continua\n",main_text), col="purple", ylim=c(0,1), xlab="x", ylab="F(x)")

  axis (1, ascisse )
  axis (2, format (c(0,frequenze_relative_cumulative), digits = 2), las =2)
  box ()
}

funzione_di_distribuzione_empirica_continua(parcheggio, "per la difficoltà nel trovare parcheggio")
funzione_di_distribuzione_empirica_continua(trasporto_pubblico, "per la difficoltà di collegamento con mezzi pubblici")
funzione_di_distribuzione_empirica_continua(traffico, "per il traffico")
funzione_di_distribuzione_empirica_continua(illuminazione_stradale, "per la scarsa illuminazione stradale")
funzione_di_distribuzione_empirica_continua(condizioni_stradali, "per le cattive condizioni delle strade")