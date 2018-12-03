################################################################################
# Funzione per la ricerca dei toni puri
# (tabella ISO 226 1987 per campo libero)
# versione: 1.0.0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data analyst: Pasquale Scordino
# Luogo: Biella - Dipartimento ARPA
# Data: 25/10/2018
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x è un dataframe con variabili, date e frequenze llmin
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Costruzione funzione
search.tone <- function(x, plot.tone = FALSE) {
  
  if(!exists("iso")){
  cat("Attenzione! non trovo la tabella iso.rda \n")
  load("iso.rda")
    }

  if(!exists("energetic.mean", mode = "function")){
    cat("Attenzione! non trovo la funzione energetic.mean.R \n")
    source("energetic.mean.R")
    }

  names(x)[2:dim(x)[2]] <- c(
    "6.3",
    "8",
    "10",
    "12.5",
    "16",
    "20",
    "25",
    "31.5",
    "40",
    "50",
    "63",
    "80",
    "100",
    "125",
    "160",
    "200",
    "250",
    "315",
    "400",
    "500",
    "630",
    "800",
    "1k",
    "1.25k",
    "1.6k",
    "2k",
    "2.5k",
    "3.15k",
    "4k",
    "5k",
    "6.3k",
    "8k",
    "10k",
    "12.5k",
    "16k",
    "20k"
  )
   # Seleziono il dataframe senza la colonna date 
   x <- x[, 2:dim(x)[2]]

   # Uso la funzione gather per invertire la tabella da wide a long
  xlong  <- tidyr::gather(x, freq, llmin)
  Tab.min <- as.data.frame(as.table(tapply(
    xlong$llmin,
    xlong$freq, energetic.mean
  )))
   # Nomino le colonne della nuova tabella
  names(Tab.min) <- c("Hz", "LLfmin")
  Tab.min$Hz  <- as.factor(Tab.min$Hz)
  Tab.min$Hz <- factor(Tab.min$Hz, levels = c(
     "6.3",
     "8",
     "10",
     "12.5",
     "16",
     "20",
     "25",
     "31.5",
     "40",
     "50",
     "63",
     "80",
     "100",
     "125",
     "160",
     "200",
     "250",
     "315",
     "400",
     "500",
     "630",
     "800",
     "1k",
     "1.25k",
     "1.6k",
     "2k",
     "2.5k",
     "3.15k",
     "4k",
     "5k",
     "6.3k",
     "8k",
     "10k",
     "12.5k",
     "16k",
     "20k"
   ))

   Tab.min <- with(Tab.min, Tab.min[order(Hz, LLfmin),])

  # Manipolazione dataset
  Tab.min$LLfmin <-
    suppressWarnings(as.numeric(as.character(Tab.min$LLfmin)))
  isof <- iso
  isof$Hz <- Tab.min$Hz
  isof$LLfmin <- signif(x = Tab.min$LLfmin, digit = 3)
  isof$afC <- (isof$af) * (isof$LLfmin - isof$Tf)
  isof$bfC <- (isof$bf) * (isof$LLfmin - isof$Tf)
  isof$Ln <- (isof$afC) / (1 + isof$bfC) + 4.2
  isof$Ln1 <- max(isof$Ln, na.rm = TRUE)
  isof$A <- (isof$Ln1) - 4.2
  isof$Lf <-
    signif(x = (isof$A) / (isof$af - (isof$A) * (isof$bf)) + isof$Tf,
           digit = 3)
  isof <- as.data.frame(isof)
  
  # Ricerca Tono puro
  A <- (length(isof$Hz[(which(isof$LLfmin >= isof$Lf))]) == 1)
  B <-
    (isof$Hz[(which(isof$LLfmin >= isof$Lf))] == isof$Hz[(which(diff(isof$LLfmin) >=
                                                                  5) + 1)])
  
  if (A & A %in% B) {
    tonHz <-
      paste(isof$Hz[(which(isof$LLfmin >= isof$Lf))], "Hz", sep = "")
  } else {
    tonHz <- "NA"
  }
  
  # Livello LLfmin del Tono
  if (tonHz == "NA") {
    LevHz <- "NA"
  } else {
    LevHz <- paste(isof$LLfmin[(which(isof$LLfmin >= isof$Lf))],
                   "dB    ", sep = " ")
  }
  
  # Valore isofonica
  if (tonHz == "NA") {
    LevIsof <- "NA"
  } else {
    LevIsof <- paste(signif(max(isof$Ln, na.rm = TRUE), digit = 3),
                     "phon", sep = " ")
  }
  
  # Grafico: copia su cartella di lavoro e mostra su device
  if (plot.tone == TRUE) {
    x11()
    par(mar = c(5, 5, 2, 1))
    isofplot <- barplot(
      isof$LLfmin,
      names.arg = isof$Hz,
      col = "lightblue",
      ylim = c(0, 100),
      main = "Grafico Isofonica più alta vs Spettro LLFmin - Campo libero",
      col.sub = "darkorange4",
      border = "darkcyan",
      xlab = "Frequenza (Hz)",
      ylab = "Livello sonoro (dB)",
      space = 0
    )
    lines(isofplot,
          isof$Lf,
          type = "l",
          col = "brown4",
          lwd = 2)
    text(
      levels(isof$Hz)[8],
      96,
      paste("Tono:", sep = " ", tonHz),
      adj = c(0, 0),
      font = 2,
      col = "darksalmon",
      cex = 0.8
    )
    text(
      levels(isof$Hz)[8],
      91,
      paste("LLfmin:", sep = " ", LevHz),
      adj = c(0, 0),
      font = 2,
      col = "darksalmon",
      cex = 0.8
    )
    text(
      levels(isof$Hz)[8],
      86,
      paste("Isofonica:", sep = " ", LevIsof),
      adj = c(0, 0),
      font = 2,
      col = "darksalmon",
      cex = 0.8
    )
    
    dev.copy2pdf(file = "plotSearchTone.pdf",
                 width = 11.69,
                 height = 8.27)
    
  } else {
    if (plot.tone == FALSE) {
      string.result <- c(
        paste("Frequenza del tono:", tonHz, sep = " "),
        paste("Livello del tono:", LevHz, sep = " "),
        paste("Isofonica:", LevIsof, sep = " ")
      )
      return(string.result)
    }
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
