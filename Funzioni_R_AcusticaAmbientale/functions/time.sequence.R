################################################################################
#  Funzione per creare un sequenza: data(YYYY-MM-DD), ora(hh:mm:ss)
#  Data analyst: P. Scordino
#  Data: 01/10/2018
#  Luogo: Dip. Arpa Piemonte Biella
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# La funzione accetta date e ore in formato "anno-mese-giorno H:M:S" 
# (2018-11-12 18:00:00), by è il passo della sequenza by = 1 (secondo)  
# La funzione restituisce una sequenza di date e ore con un passo definito da by
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
time.sequence <- function(date.start, hour.start, date.end, hour.end, by = 1){
  start.seq <- as.POSIXct(paste(date.start, hour.start, sep = " "), 
                          format("%Y-%m-%d %H:%M:%S"))
  stop.seq <- as.POSIXct(paste(date.end, hour.end, sep = " "), 
                         format("%Y-%m-%d %H:%M:%S"))
  sequence <- seq(start.seq,
                  (stop.seq - 1), by)
  return(sequence)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
