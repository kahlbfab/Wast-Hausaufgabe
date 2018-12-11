# Hausaufgabe
#### Aufgabe 1 ####

library("tidyr")
library(readr)
require(dplyr, quietly = T)
library(ggplot2)

# Daten einlesen
ugz_luftqualitaetsmessung_seit_2012 <- read_csv("ugz_luftqualitaetsmessung_seit-2012.csv")
luftqualitaet <- as_tibble(ugz_luftqualitaetsmessung_seit_2012)

# Werte und titel separieren
titel <- slice(luftqualitaet, c(2))
titel[1] <- "Datum"
werte <- slice(luftqualitaet, c(-1:-5))

# Tabelle nach ort auseinander nehmen
stampfenbach <- werte %>% select(1,2:14) %>%
  mutate(station = "Stampfenbachstrasse")
stampfenbach_titel <- titel %>% select(1,2:14)
stampfenbach_titel[15] <- "Station"
names(stampfenbach) <- stampfenbach_titel

schimmel <- werte %>% select(1,15:22) %>%
  mutate(station = "Schimmelstrasse")
schimmel_titel <- titel %>% select(1,15:22)
schimmel_titel[10] <- "Station"
names(schimmel) <- schimmel_titel

heubeer <- werte %>% select(1,23:26) %>%
  mutate(station = "Heubeeribüel")
heubeer_titel <- titel %>% select(1,23:26)
heubeer_titel[6] <- "Station"
names(heubeer) <- heubeer_titel

rosengarten <- werte %>% select(1,27:30) %>%   
  mutate(station = "Rosengarten")
rosengarten_titel <- titel %>% select(1,27:30)
rosengarten_titel[6] <- "Station"
names(rosengarten) <- rosengarten_titel

# Tabelle zusammensetzen
luftqual <- bind_rows(stampfenbach, schimmel, heubeer, rosengarten)
str(luftqual)

# Zahlen von character nach numeric wandeln
luftqual[2:14] <- as_tibble(sapply(luftqual[2:14], as.numeric))
str(luftqual)
View(luftqual)



#### Aufgabe 2 ####
# mit ggplot
gather(luftqual, key = variable, value = value, c(2,3,4,6,7,8)) %>% 
  ggplot(aes(x = Datum, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  facet_wrap(~Station)

# mit ggally #falscher Ansatz
# library(GGally)
# ggpairs(luftqual, mapping = ggplot2::aes(color = Station), columns = c(2,3,4,6,7,8),
#         upper =list(continuous = wrap("points", alpha = 0.2)))


#### Aufgabe 3 ####
library(VIM)
par(mar= c(4,2,2,2))


# Alle Nan Werte zu NA`s ersetzen
luftqual.A3 <- rapply(luftqual, f=function(x) ifelse(is.nan(x),NA,x), how="replace")
View(luftqual.A3)

# Neue Namen für Header, ansonsten kein Platz für Plot
names(luftqual.A3) <- c("Datum", "SO2", "CO", "O3_max_h1", "O3_nb_h1>120", "NO2", "NO", "PM10", "T", "Hr", "p", "WVS", "StrGlo", "RainDur", "Station")
View(luftqual.A3)

# Anteil von NA in Variable und Kombinationen von Varibablen mittels Package VIM
aggr_plot <- aggr(luftqual.A3, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(luftqual.A3), cex.axis=0.7, gap=0.5, ylab=c("Histogram of missing data","Pattern"), cex.lab = 1)

# Anzahl NA`s im Datensatz
sum(is.na(luftqual.A3))




#### Aufgabe 4 ####

# Feinstaub (PM10) jeweils fuer die Messpunkte 
# Stampfenbachstrasse, Schimmelstrasse, Rosengartenstrasse
# !! Heubeeribüel hat keine Messung von PM10 

# PM10 
# Jahresmittelgrenzwert: 20ug/m^3
# Tagesmittelgrenzwert: 50ug/m^3, darf max. 1x pro Jahr ueberschritten werden


# uberschittene Tagesmittelgrenzwerte
luftqual.PM10 <- luftqual %>% select(Datum, 'Feinstaub PM10', Station) %>%
  mutate(PM10_uberschritt = luftqual$`Feinstaub PM10` >= 50 )

#1 wie oft wird der Tagesmittel-Grenzwert an welcher Station ueberschritten?
luftqual.PM10 %>% group_by(Station) %>% summarize(n = sum(PM10_uberschritt, na.rm = T))

#2 ueberschrittenen Tagesmittelgrenzwerte fuer alle Stationen ueber die Zeit in einer Grafik
luftqual.PM10 %>% filter(PM10_uberschritt) %>% 
  ggplot(aes(x=Datum, y=`Feinstaub PM10`)) + geom_point(aes(color = Station)) + ggtitle("PM10 > 50")

# Der Tagesmittelgrenzwert wird bei allen Stationen oft an den gleichen Tagen uebertroffen.
# Dies ist gut moeglich da alle Messstatinen unter aehnlichen Einfluessen auf den Feinstaub reagieren
# wie zb. das Wetter.

#3 In welchen Jahren und Stationen ist der Anteil der Tage mit Grenzwert uberschreitungen
# signifikant groesser als zufaellig

# Jahresmittelgrenzwert = 20ug/m^3

# Messwerte sind iid: independent and identically distributed
luftqual.PM10 %>% group_by(Station) %>% summarize(n = sum(PM10_uberschritt, na.rm = T)) %>%
  mutate(wkeit_n = punif(q=n, min=0, max=365))

#### nicht sicher obe uniform Verteilung stimmt.


#### Aufgabe 5 ####

# Regendauer ~ PM10 pro Station 
# select(luftqual, Datum, Regendauer)
# PM10_temp <- luftqual %>% select(Datum,`Feinstaub PM10`, Station) %>%
  
# regendauer zu allen Stationen hinzufuegen mit leftjoin
ordered_PM10 <- left_join(x = select(luftqual, Datum,`Feinstaub PM10`, Station),
          y = filter(luftqual, Station == "Stampfenbachstrasse") %>% select( Datum, Regendauer),
          by = "Datum")
  
# Plot
ggplot(ordered_PM10, aes(x= Regendauer, y= `Feinstaub PM10`)) + geom_point(aes(color = Station))

