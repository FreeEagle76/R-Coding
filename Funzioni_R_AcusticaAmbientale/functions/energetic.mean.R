################################################################################
# Funzione "Calcolo della media energetica"
#(media logaritmica in base 10)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data analyst: Pasquale Scordino
# Luogo: Biella - Dipartimento ARPA
# Data: 25/10/2015
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x è un vettore
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creazione di una funzione personalizzata "Calcolo della media energetica
#(media logaritmica in base 10)"

energetic.mean <- function(x) {
  x <- x[!is.na(x)]
  li <- (1 / length(x))
  s <- sum(10 ^ (x / 10))
  return(round(10 * (log10(li * s)), digits = 1))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creazione di una funzione personalizzata "Calcolo della media energetica pesata
# sul tempo in H:M:S come stringa
# (media logaritmica in base 10 pesata sul tempo)"

# ATTENZIONE: La funzione elimina gli NA accoppiati e non accoppiati sui due
#             vettori x e t

energetic_w.mean <- function(x, t) {
  Index.non.accoppiati <- which((is.na(x) == is.na(t)) == FALSE)
  x[Index.non.accoppiati] <- NA
  t[Index.non.accoppiati] <- NA
  x <- x[!is.na(x)]
  t <- t[!is.na(t)]
  Res <- as.POSIXlt(paste(Sys.Date(), t))
  tm  <- (((Res$hour * 60) + Res$min) * 60) + Res$sec
  s <- sum((10 ^ (x / 10)) * tm)
  return(round(10 * (log10(s / sum(tm))), digits = 1))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

