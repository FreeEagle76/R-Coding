######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Creazione di un dataframe

a<-runif(100,1,100)
l<- sample(letters[1:21], 100, replace=TRUE)
s<-seq(1:100)
lo<-rep(c(TRUE, TRUE, FALSE, TRUE, FALSE), 20)

df.1<- data.frame(s,l,a,lo) #dataframe

m<-cbind(s,l,a,lo)          #matrice di stringhe

str(df.1) #visione della struttura del dataframe
fix(df.1) #edit del dataframe

dim(df.1) #stampa a schermo la dimensione del dataframe, osservabili e variabili

names(df.1) #stampa a video le etichette delle colonne

summary(df.1) #statistica descrittiva del dataframe


#Manipolazione del dataframe

names(df.1)<- c("Id", "Categorie", "Valori", "Vlogici") #Cambio etichette colonne
names(df.1)[4]<- "Logici" #Cambio etichetta della colonna 4

df.1$Valore.2<- runif(100, 1,100) #aggiunta di una colonna
df.1$Valore.2<- NULL #eliminazione di una colonna


df.1$Valori[88]<- NA #inserimento di un valore mancante in posizione 88
df.1$Valori[df.1$Categorie == "o"] #estrazione dei dati della colonna a con condizione
df.1[df.1$Categorie == "o", ] #estrazione di un sub dataframe dal dataframe df.1 con condizione

#oppure

subset(df.1, Categorie == "o")

########################################################################################
 
#Creazione di una lista
a<-runif(100,1,100)
l<- sample(letters[1:21], 100, replace=TRUE)
s<-seq(1:100)
lo<-rep(c(TRUE, TRUE, FALSE, TRUE, FALSE), 20)
df.1

list_p<-list(a,l,s,lo,df.1)
 
str(list_p)

list_p[2]     #estrazione del secondo oggetto dalla lista

list_p[[2]][1]#estrazione del primo elemento del secondo oggetto della lista

list_p[[5]][1,] #estrazione della prima riga del quinto oggetto della lista

########################################################################################
