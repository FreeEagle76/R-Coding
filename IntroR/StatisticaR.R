A <- rnorm(120)

hist(A)
nclassi <- round(sqrt(length(A)))
classi <- seq(min(A), max(A), length=nclassi+1)
hist(A, breaks=classi)

par(mfrow=c(1,2))
hist(A, breaks = classi ,probability = TRUE, col = "gray")
lines(density(A), col= "red", lwd=2)
boxplot(A, main="boxplot of A", col = "grey")

library("MASS")
data("geyser")
geyser
summary(geyser)
boxplot(geyser)

par(mfrow=c(1,2))
boxplot(geyser$waiting, xlab= "waiting")
boxplot(geyser$duration, xlab= "duration")

par(mfrow=c(1,2))
hist(geyser$waiting, probability = TRUE, col = "grey")
lines(density(geyser$waiting), col = "blue3")
hist(geyser$duration, probability = TRUE, col = "grey")
lines(density(geyser$duration), col = "blue3")

#Distribuzione dei dati

k <- rchisq(200, 2)
hist(k, probability = TRUE)
lines(density(k))


t <- rt(20, 19)
hist(t, probability = TRUE)
lines(density(t))


F <- rf(30, 14, 14)
hist(F, probability = TRUE)
lines(density(F))

lognorm <- rlnorm(100)
hist(lognorm, probability = TRUE)
lines(density(lognorm))

#Test statistici

#Test di t di Student ad un solo campione
A <- c(6.0, 4.4, 5.0, 5.3, 5.2, 5.8, 5.6)
t.test(A, mu=5.3)

qt(0.975, 6)#t tabulato per 6 gdl

#Test di t di Student a due campioni indipendenti
a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
t.test(a, b)

qt(0.975, 15.981)#t tabulato per 15.981 gdl


#Test di t di Student a due campioni appaiati
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

t.test(a,b, paired=TRUE)

qt(0.975, 9)


#Alcuni test per la verifica della normalità

#Lilliefors (Kolmogorov-Smirnov) test for normality

library(nortest)
lillie.test(b)


#Shapiro-Wilk normality test
shapiro.test(b)

#Outlier test
library(outliers)

dixon.test(b)# per piccoli campioni max 30

boxplot(airquality$Ozone, pars=list(outcol="red"))
boxplot.stats(airquality$Ozone)
bplo.out<-boxplot(airquality$Ozone)
str(bplo.out)

grubbs.test(airquality$Ozone)

