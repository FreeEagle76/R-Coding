#########################################################################################################
#Script per il controllo dell'accessibilità dei files pdf - Versione 1.3 - 
#
#P. Scordino - 17/11/2016
#
#########################################################################################################

library(pander)
setwd("/home/pasquale/Documenti/DocumentiPasquale/Progetti/ScriptR_PDF_Scan/PDF/")
#setwd("C:/Users/pasqscor/Desktop/PDFscan")

dir<-c("/home/pasquale/Documenti/DocumentiPasquale/Progetti/ScriptR_PDF_Scan/PDF/")
dir2<-c("/home/pasquale/Documenti/DocumentiPasquale/Progetti/ScriptR_PDF_Scan/PDF/PDF_Inaccessibili/")

listaFONT<-list()
VettorePDF.open<-c()
VettorePDF.close<-c()

filesPDF<-list.files(path =dir, all.files=FALSE, pattern = "\\.pdf$")  #all.files=FALSE considera solo i files visibili
a<-filesPDF[1:length(filesPDF)]

for (j in 1: length(a)) {
  listaFONT[[j]]<-system(paste("pdffonts", a[j], sep = " "), intern = TRUE)
  if (length(listaFONT[[j]])<= 2) {
    VettorePDF.close[j]<- 1
    file.copy(from = a[j],  to = dir2)
    unlink(a[j])
  } else    {
    VettorePDF.open[j]<- 1
  } 
  Sum.OpenPDF<- sum(VettorePDF.open, na.rm = TRUE)
  Sum.ClosePDF<- sum(VettorePDF.close, na.rm = TRUE)
  Sum.TotalPDF<- sum(Sum.OpenPDF, Sum.ClosePDF)
}

TabellaPDF<-as.data.frame(cbind(Sum.OpenPDF, Sum.ClosePDF, Sum.TotalPDF))
names(TabellaPDF)<- c("N° PDF accessibili", "N° PDF inaccessibili", "N° PDF totali")

pander(TabellaPDF)
write.table(TabellaPDF, "TabellaPDFsummary.csv", sep = ";", row.names = FALSE)
#########################################################################################################

filesPDFAccessibili<-list.files(path =dir, all.files=FALSE, pattern = "\\.pdf$")  #all.files=FALSE considera solo i files visibili
filesPDFInaccessibili<-list.files(path =dir2, all.files=FALSE, pattern = "\\.pdf$")  #all.files=FALSE considera solo i files visibili

TabFilePDFopen<- data.frame(filesPDFAccessibili, stringsAsFactors = FALSE)
names(TabFilePDFopen)<- c("Files")
TabFilePDFopen$Accessibilita<- "SI"
TabFilePDFclose<-data.frame(filesPDFInaccessibili, stringsAsFactors = FALSE)
names(TabFilePDFclose)<- c("Files")
TabFilePDFclose$Accessibilita<- "NO"

TabFilesPDF<-rbind(TabFilePDFopen,TabFilePDFclose)
pander(TabFilesPDF)
write.table(TabFilesPDF, "TabellaPDF.csv", sep = ";", row.names = FALSE)
#########################################################################################################
