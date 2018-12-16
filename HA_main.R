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

# Wetterdaten "manipulieren" MAster Stampfenbach , Slaves andere Stationen
luftqual <- luftqual %>% group_by(Datum) %>% arrange(Datum) %>% fill(Lufttemperatur : Regendauer, .direction = "down") %>% arrange(Station)
View(luftqual)

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

# brauche die Daten in diesem Format. Was ist falsch mit den anderen?
luftqual.A4 <- bind_rows(stampfenbach, schimmel, heubeer, rosengarten)
luftqual.A4[2:14] <- as_tibble(sapply(luftqual.A4[2:14], as.numeric))

# uberschittene Tagesmittelgrenzwerte
luftqual.PM10 <- luftqual.A4 %>% select(Datum, 'Feinstaub PM10', Station) %>%
  mutate(PM10_uberschritt = luftqual.A4$`Feinstaub PM10` >= 50 )

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


# Datum nur das Jahr interessant
luftqual.PM10 %>% mutate(Jahr = strtrim(luftqual.PM10$Datum, 4)) %>%
  group_by(Jahr, Station) %>%
  summarize(n = sum(PM10_uberschritt, na.rm = T))

# t test: 
# h0: mu =  
# h1:
#t.test(daten, mu = 8.2, altern = "two.sided")


#### Aufgabe 5 ####

# Regendauer ~ PM10 pro Station 
# select(luftqual, Datum, Regendauer)
# PM10_temp <- luftqual %>% select(Datum,`Feinstaub PM10`, Station) %>%
  
# regendauer zu allen Stationen hinzufuegen mit leftjoin
ordered_PM10 <- left_join(x = select(luftqual, Datum,`Feinstaub PM10`, Station),
          y = filter(luftqual, Station == "Stampfenbachstrasse") %>% select( Datum, Regendauer),
          by = "Datum")
  
# Plot
ggplot(ordered_PM10, aes(x= Regendauer, y= `Feinstaub PM10`)) +
  geom_point(aes(color = Station),  alpha = 0.5)

# Daten fuer Test Praeparieren
PM10_test <- ordered_PM10 %>% group_by(Datum) %>%
  summarise(PM10_mean = mean(`Feinstaub PM10`, na.rm = T) ,
            Regendauer = Regendauer[1])

# tTest
t.test(x = PM10_test$PM10_mean[PM10_test$Regendauer == 0], 
       y = PM10_test$PM10_mean[PM10_test$Regendauer != 0],
       conf.level = 0.99, alternative = "greater")
# p < 0.01 --> h0 verwerfen 

##### Aufgabe 6#################
# fehler beim einlesen weil bei gewissen Antworten kommas drin sind
fuck_name <- c("intnr2015Sort","stadtkreiseLang","stadtkreiseSort","f10000Lang","f10000Sort","f20000Lang","f20000Sort","f20010Lang","f20010Sort","f21000_rec2aLang","f21000_rec2aSort","f21000_rec2bLang","f21000_rec2bSort","f21000_rec2cLang","f21000_rec2cSort","f23001Lang","f23001Sort","f23002Lang","f23002Sort","f23003Lang","f23003Sort","f23004Lang","f23004Sort","f23005Lang","f23005Sort","f23006Lang","f23006Sort","f23007Lang","f23007Sort","f23008Lang","f23008Sort","f23009Lang","f23009Sort","f23010Lang","f23010Sort","f23011Lang","f23011Sort","f23013Lang","f23013Sort","f23017Lang","f23017Sort","f23019Lang","f23019Sort","f24001Lang","f24001Sort","f24002Lang","f24002Sort","f24003Lang","f24003Sort","f24004Lang","f24004Sort","f24005Lang","f24005Sort","f24006Lang","f24006Sort","f24007Lang","f24007Sort","f24008Lang","f24008Sort","f24009Lang","f24009Sort","f24010Lang","f24010Sort","f24011Lang","f24011Sort","f24012Lang","f24012Sort","f24013Lang","f24013Sort","f24014Lang","f24014Sort","f25000Lang","f25000Sort","f25050Lang","f25050Sort","f25060Lang","f25060Sort","f25070Lang","f25070Sort","f25080Lang","f25080Sort","f25100Lang","f25100Sort","f25200Lang","f25200Sort","f25300a_recLang","f25300a_recSort","f25300b_recLang","f25300b_recSort","f25300c_recLang","f25300c_recSort","f25300d_recLang","f25300d_recSort","f26010Lang","f26010Sort","f26020Lang","f26020Sort","f26030Lang","f26030Sort","f26050Lang","f26050Sort","f26060Lang","f26060Sort","f30210Lang","f30210Sort","f30700Lang","f30700Sort","f30800Lang","f30800Sort","f31000Lang","f31000Sort","f35000Lang","f35000Sort","f35100Lang","f35100Sort","f36101Lang","f36101Sort","f36102Lang","f36102Sort","f36103Lang","f36103Sort","f36104Lang","f36104Sort","f36105Lang","f36105Sort","f36106Lang","f36106Sort","f36107Lang","f36107Sort","f36108Lang","f36108Sort","f36109Lang","f36109Sort","f36110Lang","f36110Sort","f36200Lang","f36200Sort","f36400a_recLang","f36400a_recSort","f36400b_recLang","f36400b_recSort","f36400c_recLang","f36400c_recSort","f40100Lang","f40100Sort","f40200Lang","f40200Sort","f40300Lang","f40300Sort","f40400a_recLang","f40400a_recSort","f40400b_recLang","f40400b_recSort","f40400c_recLang","f40400c_recSort","f40400d_recLang","f40400d_recSort","f40400e_recLang","f40400e_recSort","f40400f_recLang","f40400f_recSort","f40400g_recLang","f40400g_recSort","f50001Lang","f50001Sort","f50002Lang","f50002Sort","f50003Lang","f50003Sort","f50004Lang","f50004Sort","f50005Lang","f50005Sort","f50030Lang","f50030Sort","f50040Lang","f50040Sort","f50051Lang","f50051Sort","f50052Lang","f50052Sort","f50053Lang","f50053Sort","f60010Lang","f60010Sort","f60023Lang","f60023Sort","f60025Lang","f60025Sort","f60026Lang","f60026Sort","f60027Lang","f60027Sort","f60028Lang","f60028Sort","f60100Lang","f60100Sort","f60110Lang","f60110Sort","f60121Lang","f60121Sort","f60123Lang","f60123Sort","f60124Lang","f60124Sort","f60125Lang","f60125Sort","f60126Lang","f60126Sort","f60200Lang","f60200Sort","f60210Lang","f60210Sort","f60221Lang","f60221Sort","f60223Lang","f60223Sort","f60224Lang","f60224Sort","f60225Lang","f60225Sort","f60227Lang","f60227Sort","f60300Lang","f60300Sort","f60310Lang","f60310Sort","f60322Lang","f60322Sort","f60324Lang","f60324Sort","f60325Lang","f60325Sort","f60327Lang","f60327Sort","f60328Lang","f60328Sort","f60500Lang","f60500Sort","f72000Lang","f72000Sort","gewichtSort")
bev_bef <- read_delim("bevoelkerungsbefragung_2015_stadtentwicklung_zuerich.csv",
                                            quote = '"', delim = ',', skip = 1, col_names = fuck_name,
                                            locale = locale(encoding = "latin1"))
View(bev_bef)

names(bev_bef)[1] <- "Nummer"
num_change <- seq(0,242,2)
num_change


