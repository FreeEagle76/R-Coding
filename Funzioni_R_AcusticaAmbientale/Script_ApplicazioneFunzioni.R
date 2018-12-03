################################################################################
# Script R applicazione delle funzioni in ambito acustico ambientale
# Data analyst: P. Scordino
# Luogo: Arpa Piemonte  - Biella
# Data: 06/11/2018
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Carico le funzioni e i dati necessari per l'uso delle funzioni in esame
file.sources = list.files("functions/",pattern="*.R")
data.sources = list.files("data/" ,pattern="*.rda")

sapply(paste0("data/", data.sources), load, .GlobalEnv)
sapply(paste0("functions/", file.sources), source, .GlobalEnv)

# Carico dataset
x <- read.table("dataset_esempi/df16102018_in.csv", header = T, sep = ";",
                dec = ",", stringsAsFactors = F)

# manipolazione dataset
x <- x[, c(88, 50:85)]

# applicazione delle funzioni plot.spectrum e search.tone
plot.spectrum(x, type.plot = "spectrogram")
search.tone(x, plot.tone = T)

# applicazione della funzione acoustic.round su più colonne di un dataframe
mat <- x[, 2:dim(x)[2]]
apply(mat, 2, acoustic.round)


# applicazione della funzione avr.daynigth per le medie di periodo
df <- read.table("dataset_esempi/livelli_orari_noCal_Clean.csv", header = T, 
                 sep = ";", dec = ".", stringsAsFactors = F)

names(df)[1] <- "date"
tab.giorno <- avr.daynigth(df, "C1", periodo = "giorno",
                        statistica = "media energetica")


