######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Controlloe settaggio directory di lavoro
getwd()
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/ScriptCourseIntroR")
#setwd("/home/pasquale/Documenti/LavoroARPA/Data_Analysis/CorsoIntroR/ScriptCourseIntroR")


source("http://goo.gl/UUyEzD")

data("airquality")
airquality
summary(airquality$Ozone)
outlierKD(airquality, Ozone)

