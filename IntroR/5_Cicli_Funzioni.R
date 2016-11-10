######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#In R si cerca di usare i cicli il meno possibile, esistono funzioni che li sostituiscono

######################################################################################

#Ciclo for:

#Questo ciclo mi stampa a video i primi 10 numeri
for (i in 1:10) {
  i
  print(i)
}


# Creo un vettore contenete i primi 30 numeri
a <- 1:30

# Creo un vettore vuoto
asq <- NULL

for(i in 1:10) {
  asq[i] <- a[i]*a[i]
  print(asq[i])
}


#######################################################################################

#Strutture di controllo
x<- -10

if (x<0) {
  print("Numero negaativo")
}else{ 
  print("Numero positivo")
}



ifelse(x>=0, sqrt(x), "Impossibile")

#######################################################################################

#Funzioni personalizzate

scatterplot<-function(x,y){  
  par(mfrow=c(2,1))
  plot(x,y, main = "Scatterplot")
  boxplot(x,y, main="Boxplot")
   }

a<- 1:10
b<- 11:20

scatterplot(a,b)

#######################################################################################

Pasquale<-function(x,y){  
  x.m<-mean(x)
  y.m<-mean(y)
  if (x.m == y.m) {
    print("Le medie sono identiche")
  }else{
    print("Le medie sono diverse")
    }
}

a<- 1:10
b<- 11:20

Pasquale(a,a)

#######################################################################################
