######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Creazione di una matrice

m<- matrix(1:10,ncol=5)

a<- round(runif(12, 0, 100), 0)
b<- round(runif(12, 0, 100), 0)

m2<- rbind(a, b) #combina i due vettori a e b per righe
m3<- cbind(a, b) #combina i due vettori a e b per colonne

#Manipolazione di una matrice
m
class(m)
str(m)
dim(m)

m[1,2] #estrazione del valore in posizione 1,2 (prima riga e seconda colonna) della matrice m
m[1,]  #estrazione della prima riga della matrice m
m[,2]  #estrazione della seconda colonna della matrice m
m[-1,] #eliminazione di una riga della matrice m
m[,-1] #eliminazione di una colonna della matrice  m





