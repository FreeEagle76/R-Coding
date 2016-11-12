######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################


#Controlloe settaggio directory di lavoro
getwd()
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/ScriptCourseIntroR")
#setwd("/home/pasquale/Documenti/LavoroARPA/Data_Analysis/CorsoIntroR/ScriptCourseIntroR")


#Esercizio 1
#Si costruisca un vettore x di elementi 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,
#5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5 e 10. Per x si calcolino la lunghezza, la
#somma degli elementi, il prodotto degli elementi.

x<-c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10)

length(x)
sum(x)
prod(x)

#Esercizio 2
#Per il vettore x dell’Esercizio 1 si calcolino la media e la varianza
#mediante l’uso delle funzioni length e sum.

sum(x)/length(x)
mean((x-mean(x))^2)
sqrt(sum((x-mean(x))^2)/(length(x)-1))

#Esercizio 3
#Dai vettori
#x<-c(3,5,9,1,2,10,12,24,6),
#y<-c(0.15,0,0.32,0.51,0.18,0.22,0.6,0.98,0.12)
#z<-c(123,415,981,643,1080,89,46,75,910)
#si costruisca una matrice mat che ha come colonne i tre vettori. A
#partire da mat si costruisca una sottomatrice mat1 selezionando solo le
#righe che hanno valore di z maggiore di 900.


x<-c(3,5,9,1,2,10,12,24,6)
y<-c(0.15,0,0.32,0.51,0.18,0.22,0.6,0.98,0.12)
z<-c(123,415,981,643,1080,89,46,75,910)

mat<-cbind(x,y,z)
v<-c(x,y,z)
mat<- matrix(v, ncol = 3, nrow = 9)

submat<-mat[mat[,3] > 900,]

