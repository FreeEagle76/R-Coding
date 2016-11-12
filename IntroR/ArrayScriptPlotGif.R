######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################


#Ccontrollo e settaggio della directory di lavoro
getwd()
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/grafici/Gif/")
#setwd("/home/pasquale/Documenti/LavoroARPA/Data_Analysis/CorsoIntroR/grafici/Gif/")

########################################################################################

#Creazione dei grafici in seguenza

library(scatterplot3d)
mydata<- array(runif(125,1,100), c(5,5,5))
coords <-which(mydata!=0, arr.ind=T)


for (i in seq(70, 285, by = 1)) {

  jpeg(paste0("ArrayPlotGif",i,".jpg"), quality = 100, width = 1000)
  
     group = rep(1:5, each= 25)
     sp3d<-scatterplot3d(coords[,'dim1'],coords[,'dim2'],coords[,'dim3'],
           color = as.numeric(group), cex.symbols =1.3,  pch = 16,
           angle= i, main = paste("Rappresentazione grafica di un array:", i, "°", sep=" "))

  dev.off()
  
}
########################################################################################

#Unione in una Gif

#Creazione della funzione

make.mov <- function(){
   system("convert -delay 30  ArrayPlotGif*.jpg ArrayPlot.gif")
   unlink("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/grafici/Gif/ArrayPlotGif*.jpg")
 }

#Applicazione della funzione
make.mov()
