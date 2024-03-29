library(dplyr)
library(ggplot2)
library(CGPfunctions)
library(datasets)
library(gridExtra)
library(gapminder)
library(gghighlight)

setwd("C:\\Users\\monik\\Desktop\\samobojstwa")

dane = read.csv(file="dane ludzi.csv", header = TRUE, sep=";")
metoda = read.csv(file="metoda.csv", header = TRUE, sep = ";")
wiek = read.csv(file="wiek.csv", header = T, sep=";")

# 1. W jakim wieku ludzie najcz�ciej pr�buj� pope�ni� samob�jstwo?

names(wiek)[3:9]=c("<=12","13-18","19-24","25-29","30-49","50-69","70=>")
kolumny = colnames(wiek[3:9])

wiek_wektor = unlist(wiek[1,3:9])

ggplot(wiek[3:9,], aes(kolumny, wiek_wektor))+
  geom_bar(stat="identity", colour="black", fill="grey")+
  labs(x="wiek", y="liczba", title="Liczba pr�b samob�jczych w danych grupach wiekowych")

# 2. Najskuteczniejsza metoda samob�jstwa

names(metoda) [3:15]=c("zatrucie gazem/spalinami", "zatrucie �rodkami chemicznymi/toksycznymi",
                       "za�ycie �rodk�w nasennych/lek�w psychotropowych", "za�ycie innych lek�w",
                       "uszkodzenie uk�adu krwiono�nego", "samookaleczenie powierzchniowe", 
                       "rzucenie si� z wysoko�ci", "utoni�cie/utopienie si�","powieszenie si�",
                       "rzucenie si� pod pojazd", "zastrzelenie si�/u�ycie broni palnej",
                       "uduszenie si�", "samopodpalenie")
names(metoda)[18:30]=c("zgon zatrucie gazem/spalinami", "zgon zatrucie �rodkami chemicznymi/toksycznymi",
                       "zgon za�ycie �rodk�w nasennych/lek�w psychotropowych", "zgon za�ycie innych lek�w",
                       "zgon uszkodzenie uk�adu krwiono�nego", "zgon samookaleczenie powierzchniowe", 
                       "zgon rzucenie si� z wysoko�ci", "zgon utoni�cie/utopienie si�","zgon powieszenie si�",
                       "zgon rzucenie si� pod pojazd", "zgon zastrzelenie si�/u�ycie broni palnej",
                       "zgon uduszenie si�", "zgon samopodpalenie")
ogolem = colnames(metoda[3:15])
smierc = colnames(metoda[18:30])

ogolem_wektor = unlist(metoda[1,3:15]) 
smierc_wektor = unlist(metoda[1,18:30])

skutecznosc = smierc_wektor/ogolem_wektor*100
skutecznosc = round(skutecznosc, 3)


ggplot(metoda[3:15, 18:30,], aes(skutecznosc,reorder(ogolem, skutecznosc)))+
  xlim(0,100)+
  geom_col(colour="black", fill="grey")+
  labs(x="procent samob�jstw zako�czonych �mierci�[%]", y="metoda", title="Skuteczno�� wybranej metody samob�jczej")



# 3.Kt�ra metoda jest najcz�ciej stosowana?
names(metoda)[2]="og�em"

metoda_ogolem = unlist(metoda[1,3:15], use.names = FALSE) 

czestosc_metody=metoda_ogolem/metoda[1,2]*100

ggplot(metoda[3:15,], aes(czestosc_metody,reorder(ogolem, czestosc_metody)))+
  geom_col(colour="black", fill="grey")+
  labs(x="procent samob�jstw dokonanych wybran� metod�[%]", y="metoda", title="Cz�sto�� u�ywanej metody samob�jczej")

#grid.arrange(p2, p1, nrow = 1)

# 4. Kto cz�ciej pope�nia samob�jstwo kobiety czy m�czy�ni?
names(dane)[2:4]=c("og�em","m�czy�ni","kobiety")

czestosc_kobiety=dane[1,]$kobiety/dane[1,]$og�em*100
czestosc_kobiety
czestosc_mezczyzni = 100-czestosc_kobiety
czestosc_mezczyzni

plec = data.frame(
  p�e�=c("kobieta", "m�czyzna"),
  value=c(czestosc_kobiety, czestosc_mezczyzni))

ggplot(plec, aes(x="", y=value, fill=p�e�)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title="Pr�by samob�jcze z podzia�em na p�e�")

#5 Kto skuteczniej pope�nia samob�jstwa, kobiety czy m�czy�ni?
names(dane)[8:9]=c("zgon m�czy�ni", "zgon kobiety")

skutecznosc=data.frame(name=c("kobiety","m�czy�ni"), 
                       val=c(dane[1,]$`zgon kobiety`/dane[1,]$kobiety*100,
                         dane[1,]$`zgon m�czy�ni`/dane[1,]$m�czy�ni*100))

skutecznosc %>%
  ggplot(aes(x=name, y=val)) +
  geom_segment(aes(xend=name, yend=0))+
  geom_point(size=4, color="orange") +
  labs(x="p�e�", y="skuteczno��[%]", title="Skuteczno�� samob�jstw wsr�d m�czyzn i kobiet")+
  ylim(0,100)


#6 Odsetek samob�jstw w danym wojew�dztwie
dane%>%
  select(Wojew�dztwo, og�em, liczba.ludno�ci)%>%
  mutate(smiertelnosc=og�em/liczba.ludno�ci*100)%>%
  filter(Wojew�dztwo!="POLSKA")%>%
  ggplot(aes(x=smiertelnosc, y=reorder(Wojew�dztwo, smiertelnosc)))+
  geom_point(size=4, col="red")+
  labs(y="wojew�dztwo", x="�miertelno��[%]", title="Procent samob�jstw w danym wojew�dztwie")+
  gghighlight(Wojew�dztwo == "MA�OPOLSKIE")+
  xlim(0,0.075)


         