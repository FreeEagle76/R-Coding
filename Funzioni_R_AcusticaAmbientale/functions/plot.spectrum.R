################################################################################
# Funzione per la costruzione di uno spettro/spettrogramma
# versione: 1.0.0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data analyst: Pasquale Scordino
# Luogo: Biella - Dipartimento ARPA
# Data: 25/10/2018
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x è un dataframe con variabili date e frequenze lleq/llmin
# type.plot = "spectrum" o "spectrogram"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Costruzione funzione

plot.spectrum <- function(x, type.plot = "spectrum"){
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

   DATE <- x[, 1]
   x <- x[, 2:dim(x)[2]]
   
    if (!exists("energetic.mean", mode = "function", envir = .GlobalEnv)) {
    source("energetic.mean.R")
    }
   
   xlong  <- tidyr::gather(x, freq, llmin)
   Tab.min <- as.data.frame(as.table(tapply(
     xlong$llmin,
     xlong$freq, energetic.mean
   )))
  
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
  if (type.plot == "spectrum") {
    x11()
    par(mar = c(5, 5, 2, 1))
    isofplot <- barplot(
      Tab.min$LLfmin,
      names.arg = Tab.min$Hz,
      col = "lightblue",
      ylim = c(0, 100),
      main = "Spectrum",
      col.sub = "darkorange4",
      border = "darkcyan",
      xlab = "Frequenza (Hz)",
      ylab = "Livello sonoro (dB)",
      space = 0
    )
    
    dev.copy2pdf(file = "plotSpectrum.pdf",
                 width = 11.69,
                 height = 8.27)
  } else {
    if (type.plot == "spectrogram") {
      VettoreF<-c(6.3,8,10,12.5,16,20,25,31.5,40,50,63,80,100,125,160,200,250,
                  315,400,500,630,800,1000,1250,1600,2000,2500,3150,4000,5000,
                  6300,8000,10000,12500,16000,20000)
      Matrice <- x
      names(Matrice)[1:dim(Matrice)[2]] <- VettoreF
      VettoreT<-strftime(as.character(DATE), "%d-%b %H:%M")
      MatriceS<-as.matrix(Matrice[,1:ncol(Matrice)])
      x<-1:nrow(MatriceS)
      y<-1:ncol(MatriceS)
      
      x11()
      filled.contour(x, y, MatriceS, color = topo.colors,
                     plot.title = title(main = "Spectrogram",
                                        xlab = "Data", ylab = "Frequenza (Hz)"),
                     plot.axes = { axis(1, at=x, labels=VettoreT, las=0)
                       axis(2, at=y, labels=VettoreF) },
                     key.title = title(main = "Leq\n(dB)\n"),
                     key.axes = axis(4, seq(20, 80, by = 5))) 
      
      dev.copy2pdf(file = "plotSpectrogram.pdf",
                   width = 11.69,
                   height = 8.27)
    }
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
