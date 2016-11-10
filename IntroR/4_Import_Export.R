######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################


#Importazione file .csv e trasformazione in un dataframe

read.csv()

read.csv2()
######################################################################################

#Importazione file di testo .txt e trasformazione in un dataframe

read.table()
######################################################################################

#Importazione fogli excel("Office") .xls .xlsx o calc("libreOffice") .ods e trasformazione in un dataframe

install.packages()
library(gdata)

read.xls()


library(xlsx)

read.xlsx()
######################################################################################

library(readODS)

read.ods()  #Importa tutti i fogli di un file ods in uns lista di dataframe

read_ods()  #Importa un solo foglio di un file .ods in un dataframe
######################################################################################

#Esportazione dati in csv, txt o xlsx

write.csv()

write.csv2()

write.table()

write.xlsx()

write.xlsx2()

#Esportazione grafici come pdf, png, jpeg

a<- runif(50, 1, 100)

pdf("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/grafici/plot1.pdf")
plot(a)
dev.off()

png("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/grafici/plot2.png")
plot(a)
dev.off()

jpeg("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/grafici/plot3.jpeg")
plot(a)
dev.off()
######################################################################################

