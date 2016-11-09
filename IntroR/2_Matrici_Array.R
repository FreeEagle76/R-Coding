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
m[1, ]  #estrazione della prima riga della matrice m
m[,2 ]  #estrazione della seconda colonna della matrice m
m[-1,] #eliminazione di una riga della matrice m
m[,-1] #eliminazione di una colonna della matrice  m

m[c(1,2),c(3,4,5)] #estrazione di una sottomatrice della matrice m

m[m>6] #estrazione degli elementi >6 della matrice m (si perde una dimensione)

matrix(m[m>6],2 ,2) #estrazione degli elementi >6 della matrice m

#######################################################################################

#Rappresentazione grafica di una matrice

mydata.m<- matrix(runif(25,1,100), c(5,5))
coords.m <-which(mydata.m!=0, arr.ind=T)
sp<-plot(coords.m, col = "lightslateblue", cex=1.3,  pch = 16,
         main = "Rappresentazione grafica di una matrice")

group = rep(1:5, each= 5)
sp<-plot(coords.m, col = group, cex = 1.3,  pch = 16,
         main = "Rappresentazione grafica di una matrice")


#######################################################################################
###############################################################################################

#Creazione di un array

w<- array(1:24, c(2,3,4))


w[1, 1, 4]  #estrazione di un elemento dell'array w

w[1,  ,  ] #estrazione della prima matrice 3*4 dall' array w

w[ , 1,  ] #estrazione della prima matrice 2*4 dall' array w

w[ ,  ,1 ] ##estrazione della prima matrice 2*3 dall' array w


A<-array(runif(125, 1, 100), c(5,5,5))

A1<-A[1, ,  ] #estrazione della prima matrice 5*5 dall' array w

A2<-A[ , 1, ] #estrazione della prima matrice 5*5 dall' array w

A3<-A[ ,  ,1] ##estrazione della prima matrice 5*5 dall' array w

#######################################################################################

#Rappresentazione grafica di un array

library(scatterplot3d)
mydata<- array(runif(125,1,100), c(5,5,5))
coords <-which(mydata!=0, arr.ind=T)
sp3d<-scatterplot3d(coords[,'dim1'],coords[,'dim2'],coords[,'dim3'],
                    color = "lightslateblue", cex.symbols =1.3,  pch = 16,
                    angle = 70, main = "Rappresentazione grafica di un array")


group = rep(1:5, each= 25)
sp3d<-scatterplot3d(coords[,'dim1'],coords[,'dim2'],coords[,'dim3'],
                    color = as.numeric(group), cex.symbols =1.3,  pch = 16,
                    angle= 70, main = "Rappresentazione grafica di un array")
#######################################################################################
