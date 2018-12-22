# Hausaufgabe
#### Aufgabe 1 ####
# Alle librarys einlesen
library("tidyr")
library(readr)
require(dplyr, quietly = T)
library(ggplot2)
library(ggpubr)
library(VIM)
library(stringr)

# Daten einlesen
ugz_luftqualitaetsmessung_seit_2012 <- read_csv("ugz_luftqualitaetsmessung_seit-2012.csv")
luftqualitaet <- as_tibble(ugz_luftqualitaetsmessung_seit_2012)

# Werte und Titel separieren
titel <- slice(luftqualitaet, c(2))
titel[1] <- "Datum"
werte <- slice(luftqualitaet, c(-1:-5))

# Tabelle nach Ort aufteilen
stampfenbach <- werte %>% select(1,2:14) %>%
  mutate(Station = "Stampfenbachstrasse")
stampfenbach_titel <- titel %>% select(1,2:14)
stampfenbach_titel[length(stampfenbach)] <- "Station"
names(stampfenbach) <- stampfenbach_titel

schimmel <- werte %>% select(1,15:21) %>%
  mutate(Station = "Schimmelstrasse")
schimmel_titel <- titel %>% select(1,15:21)
schimmel_titel[length(schimmel)] <- "Station"
names(schimmel) <- schimmel_titel

heubeer <- werte %>% select(1,22:25) %>%
  mutate(Station = "Heubeeribüel")
heubeer_titel <- titel %>% select(1,22:25)
heubeer_titel[length(heubeer)] <- "Station"
names(heubeer) <- heubeer_titel

rosengarten <- werte %>% select(1,26:30) %>%   
  mutate(Station = "Rosengarten")
rosengarten_titel <- titel %>% select(1,26:30)
rosengarten_titel[length(rosengarten)] <- "Station"
names(rosengarten) <- rosengarten_titel

# Tabellen zusammensetzen
luftqual <- bind_rows(stampfenbach, schimmel, heubeer, rosengarten)

# Wetterdaten "manipulieren" Master Stampfenbach , Slaves andere Stationen
luftqual <- luftqual %>% group_by(Datum) %>% arrange(Datum) %>% 
  fill(Lufttemperatur : Regendauer, .direction = "down") %>% arrange(Station)

# Zahlen von character nach numeric wandeln
luftqual[2:14] <- as_tibble(sapply(luftqual[2:14], as.numeric))
str(luftqual)
View(luftqual)



#### Aufgabe 2 ####
# Theme um die Legenden grösse anzupassen
t <- theme(legend.title=element_text(size=7), legend.text=element_text(size=7), 
           axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
           axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Plots pro Variable
g1 <- ggplot(luftqual, aes(x=Datum, y=Schwefeldioxid)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) + t
g2 <- ggplot(luftqual, aes(x=Datum, y=Kohlenmonoxid)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) + t
g3 <- ggplot(luftqual, aes(x=Datum, y=Stickstoffmonoxid)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) + t
g4 <- ggplot(luftqual, aes(x=Datum, y=Stickstoffdioxid)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) + t
g5 <- ggplot(luftqual, aes(x=Datum, y=`Feinstaub PM10`)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) +t
g6 <- ggplot(luftqual, aes(x=Datum, y=`Ozon, höchstes Stundenmittel`)) + 
  geom_line(aes(color=Station), alpha = 0.75, size = 0.25) + t

# Zusammenfügen der Plots
ggpubr::ggarrange(g1,g2,g3,g4,g5,g6, ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")



#### Aufgabe 3 ####
# Alle NAN Werte durch NA`s ersetzen
luftqual.A3 <- rapply(luftqual, f=function(x) ifelse(is.nan(x),NA,x), how="replace")

# Neue Namen für Header, ansonsten kein Platz für Plot
names(luftqual.A3) <- c("Datum", "SO2", "CO", "O3_max_h1", "O3_nb_h1>120",
                        "NO2", "NO", "PM10", "T", "Hr", "p", "WVS", "StrGlo", "RainDur", "Station")

# Anteil von NA in Variable und Kombinationen von Varibablen mittels Package VIM
par(mar= c(4,2,2,6))
aggr_plot <- aggr(luftqual.A3, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(luftqual.A3),
                  cex.axis=0.7, gap=0.5,
                  ylab=c("Histogram of missing data","Pattern"), cex.lab = 1)

# Anzahl NA`s im Datensatz
sum(is.na(luftqual.A3))



#### Aufgabe 4 ####
# Daten umformen für alle Feinstaubwerte über 50
luftqual.PM10 <- luftqual %>% ungroup() %>%
  select(Datum, 'Feinstaub PM10', Station) %>%
  filter(Station != "Heubeeribüel") %>%         # nicht relevant da keine Messwerte
  mutate(PM10_uberschritt = `Feinstaub PM10` >= 50 )

# Anzahl ueberschrittener Tagesmittel-Grenzwerte pro Station
luftqual.PM10 %>% group_by(Station) %>% summarize(n = sum(PM10_uberschritt, na.rm = T))


# ueberschrittenen Tagesmittelgrenzwerte fuer alle Stationen ueber die Zeit in einer Grafik
luftqual.PM10 %>% filter(PM10_uberschritt) %>% 
  ggplot(aes(x=Datum, y=`Feinstaub PM10`)) + geom_point(aes(color = Station)) + ggtitle("PM10 > 50")

# Der Tagesmittelgrenzwert wird bei allen Stationen oft an den gleichen Tagen uebertroffen.
# Dies ist gut moeglich da alle Messstatinen unter aehnlichen Einfluessen auf den Feinstaub reagieren
# wie zb. das Wetter.
# Im Winter eindeutig mehr Feinstaubbelastung


#3 In welchen Jahren und Stationen ist der Anteil der Tage mit Grenzwert uberschreitungen
# signifikant groesser als zufaellig

# Jahresmittelgrenzwert = 20ug/m^3


# Datum nur das Jahr interessant
luftqual.PM10.2 <- luftqual.PM10 %>% 
  mutate(Jahr = strtrim(luftqual.PM10$Datum, 4)) %>%
  drop_na() %>%
  group_by(Jahr, Station) %>%
  summarize(n = sum(PM10_uberschritt, na.rm = T))

# poisson test: 
# h0: keine überschreitungen pro Jahr 
# h1: mehr als 1 überschreitungen pro Jahr
fun_A4 <- function(anzahl){
  testen <- binom.test(x = anzahl, n = 365, p = 1/365, alternative = "greater", conf.level = 0.99)
  return(ifelse(testen$p.value<0.01, "H1", "H0"))
}
luftqual.PM10.2$h <- apply(luftqual.PM10.2[,3], 1, fun_A4) 
# h0 wird ab 5 verworfen

# Plot dazu
ggplot(luftqual.PM10.2, aes(x = Jahr, y = n, fill = Station)) + 
  geom_col(aes(), position = "dodge") + 
  geom_text(aes(label=h),angle = 90, position = position_dodge(0.9), hjust = 1, size = 4) +
  geom_hline(yintercept = 4.5, col = "red")


#### Aufgabe 5 ####
# Regendauer zu allen Stationen hinzufuegen
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

# tTest ob zusammenhang besteht zwischen Regen und Feinstaub
# h0: Feinstaubbelastung an Tagen mit und ohne Regen gleich hoch
t.test(x = PM10_test$PM10_mean[PM10_test$Regendauer != 0], 
       y = PM10_test$PM10_mean[PM10_test$Regendauer == 0],
       conf.level = 0.99, alternative = "less")

# p < 0.01 --> h0 verwerfen 
# An Tagen mit Regen ist die Feinstaubbelastung kleiner als an Tagen ohne Regen



#### Aufgabe 6 ####
# Daten einlesen
bev_bef <- read_csv("bevoelkerungsbefragung_2015_stadtentwicklung_zuerich.csv")
adressen <- read_csv("adressen.csv")
attribute <- read_csv("attributbeschreibung_bvb_2015_stadtentwicklung_zuerich.csv")

# Welche Frage ist gesucht...
filter(attribute, str_detect(feldbeschreibung, "Grün"))
attribute$feldbeschreibung[attribute$technischerfeldname == "f23003Lang"]
attribute$feldbeschreibung[attribute$technischerfeldname == "f26050Lang"]
attribute$feldbeschreibung[attribute$technischerfeldname == "f36105Lang"] #trifft am ehsten zu
# folgende Frage ist gesucht: f36105

# Suche nach Quartier und Kreis
filter(adressen, str_detect(lokalisationsname, "Stampfenbachstrasse")) # => Kreis 1&6 => 1,5
filter(adressen, str_detect(lokalisationsname, "Schimmelstrasse")) # => Kreis 3&4 => 2,3
filter(adressen, str_detect(lokalisationsname, "Rosengarten")) # => Kreis 10 => 9
#Heuberribüel hat keine Feinstaubmessung

# Noten 1-3 neu 3
bev_bef_gruen <- select(bev_bef, 
                        intnr2015Sort, stadtkreiseLang, stadtkreiseSort, f36105Lang, f36105Sort)
bev_bef_gruen$f36105Sort[bev_bef_gruen$f36105Sort < 3] <- 3

# entfernen von weiss nicht 98 und keine Angabe 99
bev_bef_gruen <- bev_bef_gruen[!bev_bef_gruen$f36105Sort %in% c(98,99),]

# Kreise sortieren und zuweisen
kreis_stampfen <- filter(bev_bef_gruen, stadtkreiseSort %in% c(1,5))
kreis_schimmel <- filter(bev_bef_gruen, stadtkreiseSort %in% c(2,3))
kreis_rosen <- filter(bev_bef_gruen, stadtkreiseSort == 9)

# auswertung
mean(kreis_rosen$f36105Sort)
mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Rosengarten"], na.rm = T)
mean(kreis_schimmel$f36105Sort)
mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Schimmelstrasse"], na.rm = T) 
mean(kreis_stampfen$f36105Sort)
mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Stampfenbachstrasse"], na.rm = T)

data.frame(Station = c("Rosengarten", "Schimmelstrasse", "Stampfenbachstrasse"),
                 `Mittelwert Noten` = c(mean(kreis_rosen$f36105Sort), mean(kreis_schimmel$f36105Sort), mean(kreis_stampfen$f36105Sort)),
                 `Mittelwert PM10` = c(mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Rosengarten"], na.rm = T),
                                       mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Schimmelstrasse"], na.rm = T), 
                                       mean(luftqual$`Feinstaub PM10`[luftqual$Station == "Stampfenbachstrasse"], na.rm = T)))

boxplot(kreis_rosen$f36105Sort, kreis_schimmel$f36105Sort, kreis_stampfen$f36105Sort,
        names = c("Rosengarten", "Schimmelstrasse", "Stampfenbachstrasse"))

# => Schimmelstrasse hat höheren Feinstaub durchschnitt als die anderen


# wilcox.test ob die Befragten gleich zufrieden sind
# h0: Es gibt keinen Zufriedenheitsunterschied zwischen den Stadtkreisen 

wilcox.test(x = kreis_rosen$f36105Sort, y = kreis_schimmel$f36105Sort,
       conf.level = 0.99, alternative = "two.sided")
# h0 abgelehnt
# nicht gleich zufrieden

wilcox.test(x = kreis_rosen$f36105Sort, y = kreis_stampfen$f36105Sort,
       conf.level = 0.99, alternative = "two.sided")
# h0 bleibt
# gleich zufrieden

wilcox.test(x = kreis_schimmel$f36105Sort, y = kreis_stampfen$f36105Sort,
       conf.level = 0.99, alternative = "two.sided")
# h0 abgelehnt
# nicht gleich zufrieden

# Schimmelstrasse sind unzufriedener und das mittel der Feinstaubbeobachtung ist auch hoeher. 
