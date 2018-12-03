################################################################################
# Funzione per il calcolo delle medie energetiche/aritmetiche periodo diurno
# e notturno
# Diurno: 06:00:00 - 22:00:00, Notturno: 22:00:00 - 06:00:00
# versione: 1.0.0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data analyst: Pasquale Scordino
# Luogo: Biella - Dipartimento ARPA
# Data: 25/10/2018
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x è un dataframe 
# variabile è il vettore colonna di interesse,
# periodo può essere "giorno" o "notte" 
# statistica può essere "media" o "media energetica"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creazione funzione
avr.daynigth <- function(x,
                         variabile,
                         periodo = "giorno",
                         statistica = "media",
                         ...) {
  require(lubridate)
  # Inserisco i flag "giorno" (6-22) e "notte" (22-6)
  # Manipolazione data e ora
  x$date <- as.character(x$date)
  x$date <- as.POSIXct(x$date)
  x$data <- date(x$date)
  x$ora <- hour(x$date)
  x$giorno <- day(x$date)
  x$mese <- month(x$date)
  x$anno <- year(x$date)
  
  # Inserimento flags nella nuova variabile Periodo
  x$periodo1 <- ifelse(x$ora <= 5, "N", "G")
  x$periodo2 <- ifelse(x$ora >= 22, "N", "G")
  x$periodo <-
    ifelse(x$periodo1 == "N" | x$periodo2 == "N", "N", "G")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subsetting df rispetto i flags notte "N" e giorno "G"
  x_N <- subset(x, periodo == "N")
  x_G <- subset(x, periodo == "G")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cambio etichetta al dataset da "x_G" a "x"
  x <- x_G
  
  # Creazione di una funzione personalizzata "Calcolo della media energetica
  #(media logaritmica in base 10)"
  energetic.mean <- function(x) {
    x <- x[!is.na(x)]
    li <- (1 / length(x))
    s <- sum(10 ^ (x / 10))
    return(round(10 * (log10(li * s)), digits = 1))
  }
  
  if (periodo == "giorno" & statistica == "media") {
    # Calcolo media, min, massimo e deviazione standard periodo Diurno
    t_G <- tapply(x[[variabile]], list(x$giorno, x$mese, x$anno),
                  function(x) {
                    c(
                      MEAN = mean(x),
                      MIN = min(x),
                      MAX = (max(x)),
                      SD = sd(x)
                    )
                  })
    # Assemblo il contenuto della lista "t_G" e lo trasformo in dataframe
    x_G_Day <- as.data.frame(do.call(rbind, t_G))
    
    # Applico la funzione "round"
    x_G_Day <- round(x_G_Day[ , 1:4], 2)

    # Inserisco il vettore data nel dataframe
    x_G$data <- as.POSIXct(x_G$data)
    x_G_Day$DATA <- seq.Date(date(x_G$data[1]), 
                             date(x_G$data[dim(x_G)[1]]), by = "day")

    # Riordino posizione variabili
    x_G_Day <- x_G_Day[, c(5, 1:4)]
    return(x_G_Day)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  } else {
    if (periodo == "notte" & statistica == "media") {
      # codice per calcolo statistiche notturne a cavallo di due giorni
      
      # Inserimento flags alle ore notturne divise per >= 22 "N1" il resto "N2"
      x_N$PeriodoAcu <- ifelse(x_N$ora >= 22, "N1", "N2")
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x_N1 <- x_N[!(x_N$data == unique((x_N$data))[1] & x_N$ora < 22), ]
      
      end <- length(unique(x_N1$data))
      x_N2 <- x_N1[!(x_N1$data == unique((x_N1$data))[end] &
                       x_N1$ora >= 22), ]
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ciclo che mette in lista i dataframe subsettati secondo i criteri 
      # 22-6 di due giorni consecutivi
      a <- list()
      for (i in 1:length(unique(x_N2$data))) {
        a[[i]] <- subset(x_N2,
                         ora %in% c(23, 22, 0, 1, 2, 3, 4, 5) &
                           x_N2$data %in% c(unique((x_N2$data))[i],
                                            unique((x_N2$data))[i + 1]))
        a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i] &
                             a[[i]]$ora %in% c(0, 1, 2, 3, 4, 5)), ]
        
        a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i + 1] &
                             a[[i]]$ora %in% c(23, 22)), ]
      }
      
      # funzione personalizzata per il calcolo delle statistiche di base
      summary_night <- function(x) {
        c(
          MEAN = mean(x),
          MIN = min(x),
          MAX = max(x),
          SD = sd(x)
        )
      }
      
      # Applicazione funzione sulla lista contenente i dataframe 22-6
      b <- list()
      suppressWarnings(for (j in 1:length(a)) {
        b[[j]] <- summary_night(a[[j]][[variabile]])
      })
      
      # Creazione del dataframe finale con i risultati
      x_nigthACU <- as.data.frame(do.call(rbind, b))
      
      # Applico la funzione "round"
      x_nigthACU <- round(x_nigthACU[ ,1:4], 2)
      
      # Inserimento colonna DATA
      x_nigthACU$DATA <- unique(x_N2$data)
      
      # Riarrangiamento colonne del dataframe
      x_nigthACU <- x_nigthACU[, c(5, 1, 2, 3, 4)]
      
      return(x_nigthACU)
      
    } else {
      if (periodo == "giorno" & statistica == "media energetica") {
        # Calcolo media, min, massimo e deviazione standard periodo Diurno
        t_G <- tapply(x[[variabile]], list(x$giorno, x$mese, x$anno),
                      function(y) {
                        c(MEAN = energetic.mean(y),
                          MIN = min(y),
                          MAX = max(y))
                      })
        
        # Assemblo il contenuto della lista "t_G" e lo trasformo in dataframe
        x_G_Day <- as.data.frame(do.call(rbind, t_G))
        
        # Applico la funzione "round"
        x_G_Day <- round(x_G_Day[, 1:3], 2)
        
        # Inserisco il vettore data nel dataframe
        x_G$data <- as.POSIXct(x_G$data)
        x_G_Day$DATA <- seq.Date(date(x_G$data[1]), 
                                 date(x_G$data[dim(x_G)[1]]), by = "day")
        
        # Riordino posizione variabili
        x_G_Day <- x_G_Day[, c(4, 1:3)]
        
        return(x_G_Day)
        
      } else {
        if (periodo == "notte" & statistica == "media energetica") {
          # codice per calcolo statistiche notturne a cavallo di due giorni
          
          # Inserimento flags alle ore notturne divise per >= 22 "N1" 
          # il resto "N2"
          x_N$PeriodoAcu <- ifelse(x_N$ora >= 22, "N1", "N2")
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # subsetting per l'eliminazione delle prime e ultime ore che non 
          # completano il range orario da analizzare
          x_N1 <- x_N[!(x_N$data == unique((x_N$data))[1] & x_N$ora < 22), ]
          
          end <- length(unique(x_N1$data))
          x_N2 <- x_N1[!(x_N1$data == unique((x_N1$data))[end] &
                           x_N1$ora >= 22), ]
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # ciclo che mette in lista i dataframe subsettati secondo i criteri 
          # 22-6 di due giorni consecutivi
          a <- list()
          for (i in 1:length(unique(x_N2$data))) {
            a[[i]] <- subset(x_N2,
                             ora %in% c(23, 22, 0, 1, 2, 3, 4, 5) &
                               x_N2$data %in% c(unique((
                                 x_N2$data
                               ))[i],
                               unique((
                                 x_N2$data
                               ))[i + 1]))
            a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i] &
                                 a[[i]]$ora %in% c(0, 1, 2, 3, 4, 5)), ]
            
            a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i + 1] &
                                 a[[i]]$ora %in% c(23, 22)), ]
          }
          
          # funzione personalizzata per il calcolo delle statistiche di base
          summary_night <- function(x) {
            c(MEAN = energetic.mean(x),
              MIN = min(x),
              MAX = max(x))
          }
          
          # Applicazione funzione sulla lista contenente i dataframe 22-6
          b <- list()
          suppressWarnings(for (j in 1:length(a)) {
            b[[j]] <- summary_night(a[[j]][[variabile]])
          })
          
          # Creazione del dataframe finale con i risultati
          x_nigthACU <- as.data.frame(do.call(rbind, b))
        
          # Applico la funzione "round"
          x_nigthACU <- round(x_nigthACU[,1:3], 2)
          
          # Inserimento colonna DATA
          x_nigthACU$DATA <- unique(x_N2$data)
          
          # Riarrangiamento colonne del dataframe
          x_nigthACU <- x_nigthACU[, c(4, 1, 2, 3)]
          
          return(x_nigthACU)
        }
      }
    }
  }
} # fine funzione
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
