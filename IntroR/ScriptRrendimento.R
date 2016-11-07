######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################
#Questo script dipende dallo script "ScriptRdatasetAlAst.R"
######################################################################################

#Calcolo rendimento orario e formazione tabelle
HCl$Giorno<-factor(HCl$Giorno)
HCl$Ora<-factor(HCl$Ora)

rendOrario<-tapply(HCl$Minuti, list(HCl$Giorno,HCl$Ora), length)

#Ciclo per trasformare la matrice dei dati orari i un vettore

rendOrarioV<-c(NULL)
for (i in 1:length(rendOrario[,1])) {
    rendOrarioV<-append(rendOrarioV, as.vector(rendOrario[i,]), after = (31*(i-1)))
}

rendOrarioV<-round((rendOrarioV/60)*100,2)

rendGiornaliero<-(as.vector(apply(rendOrario, 1, sum)))/1440*100
rendGiornaliero<-round(rendGiornaliero, 2)

#Ciclo per trasformare la matrice dei dati orari i un vettore

MediaOrariaV<-c(NULL) 
for (i in 1:length(MediaOraria[,1])) {
    MediaOrariaV<-append(MediaOrariaV, as.vector(MediaOraria[i,]), after = (24*(i-1)))
}

#MediaOrariaV

#Creazione dataframe 
MediaOrariaHCl<-data.frame(Date, MediaOrariaV, rendOrarioV)
write.csv(MediaOrariaHCl, "MediaOrariaHCl.csv", row.names = FALSE)
  
DateG<-seq.Date(as.Date("2016-05-01"), as.Date("2016-05-31"), by = "days")

#Creazione dataframe
MediaGiornalieraHCl<-data.frame(DateG, MediaGiornaliera ,rendGiornaliero)
write.csv(MediaGiornalieraHCl, "MediaGiornalieraHCl.csv", row.names = FALSE)
#pander(MediaGiornalieraHCl)



#Controllo condizioni e avvisi

#Controllo su dati orari
library("pander")
if (min(MediaOrariaHCl$rendOrarioV) < 100) {
  f<-length(which(MediaOrariaHCl$rendOrarioV < 100))
  print(paste("Attenzione ci sono:", f ,"rendimenti orari sotto il 100% - HCl Maggio 2016", sep = " "))
  pander(subset(MediaOrariaHCl, MediaOrariaHCl$rendOrarioV < 100))
  } else {
    print("Tutto ok!!!")
  }

#Controllo su dati giornalieri 
if (min(MediaGiornalieraHCl$rendGiornaliero) < 100) {
  f<-length(which(MediaGiornalieraHCl$rendGiornaliero < 100))
  print(paste("Attenzione ci sono:", f ,"rendimenti giornalieri sotto il 100% - HCl Maggio 2016", sep = " "))
  pander(subset(MediaGiornalieraHCl, MediaGiornalieraHCl$rendGiornaliero < 100))
} else {
  print("Tutto ok!!!")
}

###################################################################################################
