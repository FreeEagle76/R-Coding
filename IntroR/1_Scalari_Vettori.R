######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Operazioni preliminari

#Controllo e settaggio della directory di lavoro
getwd()
dir()
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/ScriptCourseIntroR")
#setwd("/home/pasquale/Documenti/LavoroARPA/Data_Analysis/CorsoIntroR/ScriptCourseIntroR")

ls() #controllo presenza di oggetti nello spazio di lavoro
rm(a) # cancellazione di oggetti singoli
rm(list = c(a,b)) #cancellazione di oggetti multipli
rm(list = ls()) # cancellazione di tutti gli oggetti

#SCALARI E VETTORI
a<-5 
class(a)
b<-1.56
class(b)
c<-"Pasquale"
c.2<-"Scordino"
class(c)
d<- TRUE
class(d)

#assegnazione multipla di un valore

x<-y<-z<-5

#Operazioni con gli scalari numerici
a+b
a-b
a*b
a/b
a^b
sqrt(a)                         
sin(a)  
cos(a)   
tan(a)               
asin(a)  
acos(a)  
atan(a)              
exp(a)                      
sqrt(a)
log(a)
log10(a)

#Operazione con gli scalari caratteri
paste(c,c.2)
paste0(c,c.2)
#######################################################################################

#Vettori

a<- c(23, 45, 67, 88, 43)
e<- c(66,88,55,44,11,22)
f<- runif(10, 0, 100)
g<- seq(0, 10, 2)
h<- rep(1:5, 2)
i<- (1:10)

class(a)

b<- c("A", "B", "C", "D")
class(b)

c<- c(TRUE, FALSE, TRUE, FALSE)
class(c)

d<- as.factor(c(1,2,3,4,5))
class(d)

#Manipolazione dei vettori

a[6]<- 25 #aggiunta di un valore in posizione 6 nel vettore a

a<- a[-1] #cancellazione del primo valore nel vettore a

a[a>50] #estrae i valori maggiori di 50 dal vettore a

sort(a) #ordina i valori del vettore a in ordine crescente

sort(a, decreasing = TRUE) # ordina i valori del vettore a in ordine decrescente

append(a,e) #appende/attacca alla fine del vettore a il vettore e 

which(a>50)#estrazione degli indici del vettore a che contencono valori maggiori di 50
a[which(a>50)]#estrazione dei valori del vettore a maggiori di 50
which.max(a)
which.min(a)

length(a)#restituisce la lunghezza del vettore

sum(a)/length(a) #calcolo della media dei valori del vettore a
mean(a)

min(a)
max(a)
range(a)
sd(a)

summary(a) #statistica descrittiva del vettore a
plot(a) # rappresentazione grafica del vettore a
table(h)# tabella delle frequenze del vettore h
########################################################################################
