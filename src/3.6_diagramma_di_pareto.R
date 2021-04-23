# Configurazione dell’ambiente di sviluppo
df2019 <- read.csv("../../dataset/2019 - Mobilità e condizioni delle strade - per 100 famiglie con le stesse caratteristiche.csv", sep = ";", dec = ",", row.names = 1)

colnames(df2019) <- c("Parcheggio", "Trasporto_Pubblico", "Traffico", "Illuminazione_Stradale", "Condizioni_Stradali")

regioni <- rownames(df2019)

# Diagramma di Pareto

diagramma_di_pareto <- function(vettore, main_text, lunghezza_sequenza ) {
  minimo <- floor(min(vettore))
  massimo <- ceiling(max(vettore))
  classi <- round(seq(from = minimo, to = massimo, length.out = lunghezza_sequenza),0)
  table <- table(cut(vettore, classi))
  ordered_table <- sort (table , decreasing =TRUE)
  prop_ordered_table <- prop.table(ordered_table)
  barplot <- barplot(prop_ordered_table , ylim = c(0, 1.05) , main = paste("Diagramma  di Pareto", main_text), col=heat.colors(lunghezza_sequenza-1))
  lines (barplot, cumsum ( prop_ordered_table ), type = "b", pch = 16)
  text(barplot - 0.2, cumsum ( prop_ordered_table ) + 0.03 , paste (format ( cumsum ( prop_ordered_table  ) * 100, digits = 2) , "%"))
}

parcheggio <- df2019[[1]]
trasporto_pubblico <- df2019[[2]]
traffico <- df2019[[3]]
illuminazione_stradale <- df2019[[4]]
condizioni_stradali <- df2019[[5]]

diagramma_di_pareto(parcheggio, "per la difficoltà nel trovare parcheggio", 6)
diagramma_di_pareto(trasporto_pubblico, "per la difficoltà di collegamento con mezzi pubblici", 6)
diagramma_di_pareto(traffico, "per il traffico", 6)
diagramma_di_pareto(illuminazione_stradale, "per la scarsa illuminazione stradale", 6)
diagramma_di_pareto(condizioni_stradali, "per le cattive condizioni delle strade", 6)