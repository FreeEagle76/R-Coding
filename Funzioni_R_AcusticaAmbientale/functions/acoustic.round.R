################################################################################
# Funzione per l'approssimazione a 0.5 come previsto in acustica ambientale
# D.M. 16 marzo 1998 all. B punto 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Analyst: Pasquale Scordino
# Luogo: Biella - Dipartimento ARPA
# Data: 25/10/2018
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# La funzione dopo un primo arrotondamento ad una cifra decimale controlla
# il valore relativo alla prima cifra decimale se è compreso fra 3 e 7 approssima
# a 0.5 se è minore  di 3 approssima per difetto se è maggiore di 8 approssima per
# eccesso.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Costruzione funzione acoustic.round, x è un vettore numerico
acoustic.round <- function(x){
  na.index <- which(is.na(x))
  y <- round(x = x, digits = 1)
  y <- as.character(y)
  z <- strsplit(y, "\\.")
  h <- NULL
  for (i in 1:length(z)) {
    if (z[[i]][2] %in% c("3", "4", "5") | z[[i]][2] %in% c("6", "7") ){
      h[i] <- paste(z[[i]][1], "5", sep = ".")
    } else {
      if (z[[i]][2] %in% c("1", "2", NA)) {
          h[i] <- paste(z[[i]][1], "0", sep = ".")
      } else {
        if (z[[i]][2] %in% c("8", "9")) {
            l  <- as.numeric(z[[i]][1])+1
            as.character(l)
            h[i] <- paste(l, "0", sep = ".")
    }
    }
    }
  }
  h[na.index] <- NA
  h <- as.numeric(h)
  return(h)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Nel package DescTools c'è una funzione denominata RoundTo che fa la stessa 
# cosa, anzi è piu flessibile e veloce in quanto usa un metodo matematico 
# e si può scegliere a che multiplo approssimare.
# 
# Codice della funzione RoundTo:
# 
#function (x, multiple = 1, FUN = round) 
#{
#  if (is.function(FUN)) {
#    fct <- FUN
#    FUN <- "fct"
#    FUN <- gettextf("%s", FUN)
#  }
#  return(eval(parse(text = gettextf("%s(x/multiple) * multiple", 
#                                    FUN))))
#}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~