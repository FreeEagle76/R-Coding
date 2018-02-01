################################################################################
# Script R Elaborazioni Dati Meteo per Camburzano (Giudice di Pace Biella)
# Caso: Museo del Computer
# Dati: Piverone, Massazza, Oropa Stazioni Meteo Arpa Piemonte
# Giorni: 4 Gennaio 2017 e 22 Dicembre 2016
# Autore: P. Scordino, S. Sperotto
# Data: 30 Gennaio 2018
################################################################################

# Carico librerie
library(lubridate)
library(tidyverse)
library(scales)

# Cambio cartella di lavoro
setwd("/home/pasquale/ArpaLan/ArpaBi_Acustica/RUMORE/Misure_2017/B5_22_16_2016_Fondazione_Museo_del_Computer_Camburzano/RicorsoGiudicePace_2018/Datimeteo_ArpaPiemonte")

# Carico Dataset
df4Gen17 <- read.csv2("dati_4gen2017.csv", dec = ",", stringsAsFactors = FALSE)
df22Dic16 <- read.csv2("dati_22dic2016.csv", dec = ",", stringsAsFactors = FALSE)
################################################################################
################################################################################

# Elaborazioni 04 Gennaio 2017

df4Gen17_VVMassazza <- df4Gen17 %>%
  filter(codice == "VELV", stazione == "MASSAZZA") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)

df4Gen17_VVPiverone <- df4Gen17 %>%
  filter(codice == "VELV", stazione == "PIVERONE") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)


df4Gen17$stazione <- gsub("\\s*\\d","", df4Gen17$stazione)
df4Gen17_VVOropa <- df4Gen17 %>%
  filter(codice == "VELV", stazione == "OROPA") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)

# Calcolo vettore medie delle tre stazioni

df4Gen17_VVOropa_compare <- df4Gen17_VVOropa %>%
  filter(data %in% df4Gen17_VVPiverone$data)

df4Gen17_VVMassazza_compare <- df4Gen17_VVMassazza %>%
  filter(data %in% df4Gen17_VVPiverone$data)

CompareVVMean04Gen17 <- data_frame(df4Gen17_VVPiverone$data, 
                                   df4Gen17_VVPiverone$parametro, 
                                   df4Gen17_VVMassazza_compare$parametro, 
     df4Gen17_VVOropa_compare$parametro)

CompareVVMean04Gen17$VV_Media <- apply(CompareVVMean04Gen17[,c(2:4)],1,mean)
names(CompareVVMean04Gen17) <- c("data", "VV_Piverone", "VV_Massazza", 
                                 "VV_Oropa", "VV_MediaStazioni")


# Elaborazioni Grafiche 04 Gennaio 2017
ggplot() +
  geom_point(data = df4Gen17_VVMassazza, aes(data, parametro), colour = "red") +
  geom_point(data = df4Gen17_VVPiverone, aes(data, parametro), colour = "blue") +
  geom_point(data = df4Gen17_VVOropa, aes(data, parametro), colour = "black") +
  geom_point(data = CompareVVMean04Gen17, aes(data, VV_MediaStazioni), 
             colour = "green", size = 2) +
  geom_line(data = CompareVVMean04Gen17, aes(data ,VV_MediaStazioni), 
            colour = "green", size = 1) +
  geom_hline(aes(yintercept = 5), colour = "darkgreen") +
  geom_rect(data = Range_date, aes(xmin = start, xmax = end,
                                   ymin = -Inf, ymax = Inf),
            col = c("lightsalmon"),
            fill = c("lightsalmon"),
            alpha = 0.3,
            inherit.aes = FALSE) +
  #ggtitle("Dati del 04 gennaio 2017 - Staz. meteo ARPA Piemonte\n(Piverone/blue, 
  #Massaza/rosso, Oropa/nero, Media/verde)") +
  labs(title = "Dati del 04 gennaio 2017 - Staz. meteo ARPA Piemonte", 
       subtitle = "(Piverone/blu, Massazza/rosso, Oropa/nero, Media_Stazioni/verde)",
       caption = "Fonte dati: Arpa Piemonte Struttura Metereologia e Clima.\n Elaborazioni: Arpa Piemonte Dipartimento Nord-Est sede di Biella Servizio di Tutela e Vigilanza.") +
  theme(plot.title = element_text(hjust=0, size=20),
        plot.subtitle = element_text(hjust=0, size=16)) +
  labs(y = "Velocità vento (m/s)", x = "Data/Ora (UTC)") + 
  annotate(geom="text", x=start_annotate, y=5.2, 
           label="Vel. vento 5 m/s (Limite misure fonometriche)",
           color="darkgreen") + 
  annotate(geom = "label", x=start_annotate, y=10, 
           label="Finestra temporale delle misure di rumore residuo",
           color="black") +
  annotate("segment", x=start_annotateStart, xend =start_annotateEnd, 
           y=9.8, yend = 6, 
           arrow = arrow(type = "closed", angle = 15)) +
  scale_x_datetime(labels = date_format("%d/%m %H"),
                   date_breaks = "2 hours")

# Calcolo dati per rettangolo semitrasparente su grafico - 04 Gennaio 2017

temp <- data.frame(
  start <- as.POSIXct(strptime(c("2017-01-04 10:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "GMT")),
  
  end <- as.POSIXct(strptime(c("2017-01-04 10:30:00"), 
                             format = "%Y-%m-%d %H:%M:%S", tz = "GMT")))

Range_date <- data.frame(
  start = as.POSIXct(temp [,1], "%Y-%m-%d %H:%M:%S"),
  end   = as.POSIXct(temp [,2], "%Y-%m-%d %H:%M:%S"))

start_annotate <- as.POSIXct("2017-01-04 3:00", "%Y-%m-%d %H:%M:%S")

start_annotateStart <- as.POSIXct("2017-01-04 04:50:00", "%Y-%m-%d %H:%M:%S")

start_annotateEnd <- as.POSIXct("2017-01-04 09:50:00", "%Y-%m-%d %H:%M:%S")

################################################################################
################################################################################
################################################################################

# Elaborazioni 22 Dicembre 2016

df22Dic16_VVMassazza <- df22Dic16 %>%
  filter(codice == "VELV", stazione == "MASSAZZA") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)

df22Dic16_VVPiverone <- df22Dic16 %>%
  filter(codice == "VELV", stazione == "PIVERONE") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)


df22Dic16$stazione <- gsub("\\s*\\d","", df22Dic16$stazione)
df22Dic16_VVOropa <- df22Dic16 %>%
  filter(codice == "VELV", stazione == "OROPA") %>%
  mutate(data = paste(data, ora)) %>%
  mutate(data = dmy_hm(data)) %>%
  select(-ora)

# Calcolo vettore medie delle tre stazioni - 22 Dicembre 2016

df22Dic16_VVOropa_compare <- df22Dic16_VVOropa %>%
  filter(data %in% df22Dic16_VVMassazza$data)

df22Dic16_VVPiverone_compare <- df22Dic16_VVPiverone %>%
  filter(data %in% df22Dic16_VVMassazza$data)

CompareVVMean022Dic16 <- data_frame(df22Dic16_VVMassazza$data, 
                                    df22Dic16_VVPiverone_compare$parametro, 
                                    df22Dic16_VVMassazza$parametro, 
                                    df22Dic16_VVOropa_compare$parametro)

CompareVVMean022Dic16$VV_Media <- apply(CompareVVMean022Dic16[,c(2:4)],1,mean)
names(CompareVVMean022Dic16) <- c("data", "VV_Piverone", "VV_Massazza", 
                                  "VV_Oropa", "VV_MediaStazioni")

################################################################################
# Elaborazioni Grafiche 22 Dicembre 2016
ggplot() +
  geom_point(data = df22Dic16_VVMassazza, aes(data, parametro), colour = "red") +
  geom_point(data = df22Dic16_VVPiverone, aes(data, parametro), colour = "blue") +
  geom_point(data = df22Dic16_VVOropa, aes(data, parametro), colour = "black") +
  geom_point(data = CompareVVMean022Dic16, aes(data, VV_MediaStazioni), 
             colour = "green", size = 2) +
  geom_line(data = CompareVVMean022Dic16, aes(data ,VV_MediaStazioni), 
            colour = "green", size = 1) +
  geom_hline(aes(yintercept = 5), colour = "darkgreen") +
  geom_rect(data = Range_date2, aes(xmin = start2, xmax = end2,
                                   ymin = -Inf, ymax = Inf),
            col = c("lightsalmon"),
            fill = c("lightsalmon"),
            alpha = 0.3,
            inherit.aes = FALSE) +
  #ggtitle("Dati del 22 dicembre 2016 - Staz. meteo ARPA Piemonte\n(Piverone/blue, 
  #Massaza/rosso, Oropa/nero, Media/verde)") +
  labs(title = "Dati del 22 dicembre 2016 - Staz. meteo ARPA Piemonte", 
       subtitle = "(Piverone/blu, Massazza/rosso, Oropa/nero, Media_Stazioni/verde)",
       caption = "Fonte dati: Arpa Piemonte Struttura Metereologia e Clima.\n Elaborazioni: Arpa Piemonte Dipartimento Nord-Est sede di Biella Servizio di Tutela e Vigilanza.") +
  theme(plot.title = element_text(hjust=0, size=20),
        plot.subtitle = element_text(hjust=0, size=16)) +
  labs(y = "Velocità vento (m/s)", x = "Data/Ora (UTC)") + 
  annotate(geom="text", x=start_annotate2, y=5.1, 
           label="Vel. vento 5 m/s (Limite misure fonometriche)",
           color="darkgreen") + 
  annotate(geom = "label", x=start_annotate2, y=4.7, 
           label="Finestra temporale delle misure di rumore ambientale",
           color="black") +
  annotate("segment", x=start_annotateStart2, xend =start_annotateEnd2, 
           y=4.61, yend = 4, 
           arrow = arrow(type = "closed", angle = 15)) +
  scale_x_datetime(labels = date_format("%d/%m %H"),
                   date_breaks = "2 hours")

# Calcolo dati per rettangolo semitrasparente su grafico - 22 Dicembre 2016

temp2 <- data.frame(
  start2 <- as.POSIXct(strptime(c("2016-12-22 15:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "GMT")),
  
  end2 <- as.POSIXct(strptime(c("2016-12-22 15:30:00"), 
                             format = "%Y-%m-%d %H:%M:%S", tz = "GMT")))

Range_date2 <- data.frame(
  start2 = as.POSIXct(temp2 [,1], "%Y-%m-%d %H:%M:%S"),
  end2   = as.POSIXct(temp2 [,2], "%Y-%m-%d %H:%M:%S"))

start_annotate2 <- as.POSIXct("2016-12-22 6:00", "%Y-%m-%d %H:%M:%S")

start_annotateStart2 <- as.POSIXct("2016-12-22 06:50:00", "%Y-%m-%d %H:%M:%S")

start_annotateEnd2 <- as.POSIXct("2016-12-22 14:50:00", "%Y-%m-%d %H:%M:%S")
################################################################################
################################################################################