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

# 1. W jakim wieku ludzie najczêœciej próbuj¹ pope³niæ samobójstwo?

names(wiek)[3:9]=c("<=12","13-18","19-24","25-29","30-49","50-69","70=>")
kolumny = colnames(wiek[3:9])

wiek_wektor = unlist(wiek[1,3:9])

ggplot(wiek[3:9,], aes(kolumny, wiek_wektor))+
  geom_bar(stat="identity", colour="black", fill="grey")+
  labs(x="wiek", y="liczba", title="Liczba prób samobójczych w danych grupach wiekowych")

# 2. Najskuteczniejsza metoda samobójstwa

names(metoda) [3:15]=c("zatrucie gazem/spalinami", "zatrucie œrodkami chemicznymi/toksycznymi",
                       "za¿ycie œrodków nasennych/leków psychotropowych", "za¿ycie innych leków",
                       "uszkodzenie uk³adu krwionoœnego", "samookaleczenie powierzchniowe", 
                       "rzucenie siê z wysokoœci", "utoniêcie/utopienie siê","powieszenie siê",
                       "rzucenie siê pod pojazd", "zastrzelenie siê/u¿ycie broni palnej",
                       "uduszenie siê", "samopodpalenie")
names(metoda)[18:30]=c("zgon zatrucie gazem/spalinami", "zgon zatrucie œrodkami chemicznymi/toksycznymi",
                       "zgon za¿ycie œrodków nasennych/leków psychotropowych", "zgon za¿ycie innych leków",
                       "zgon uszkodzenie uk³adu krwionoœnego", "zgon samookaleczenie powierzchniowe", 
                       "zgon rzucenie siê z wysokoœci", "zgon utoniêcie/utopienie siê","zgon powieszenie siê",
                       "zgon rzucenie siê pod pojazd", "zgon zastrzelenie siê/u¿ycie broni palnej",
                       "zgon uduszenie siê", "zgon samopodpalenie")
ogolem = colnames(metoda[3:15])
smierc = colnames(metoda[18:30])

ogolem_wektor = unlist(metoda[1,3:15]) 
smierc_wektor = unlist(metoda[1,18:30])

skutecznosc = smierc_wektor/ogolem_wektor*100
skutecznosc = round(skutecznosc, 3)


ggplot(metoda[3:15, 18:30,], aes(skutecznosc,reorder(ogolem, skutecznosc)))+
  xlim(0,100)+
  geom_col(colour="black", fill="grey")+
  labs(x="procent samobójstw zakoñczonych œmierci¹[%]", y="metoda", title="Skutecznoœæ wybranej metody samobójczej")



# 3.Która metoda jest najczêœciej stosowana?
names(metoda)[2]="ogó³em"

metoda_ogolem = unlist(metoda[1,3:15], use.names = FALSE) 

czestosc_metody=metoda_ogolem/metoda[1,2]*100

ggplot(metoda[3:15,], aes(czestosc_metody,reorder(ogolem, czestosc_metody)))+
  geom_col(colour="black", fill="grey")+
  labs(x="procent samobójstw dokonanych wybran¹ metod¹[%]", y="metoda", title="Czêstoœæ u¿ywanej metody samobójczej")

#grid.arrange(p2, p1, nrow = 1)

# 4. Kto czêœciej pope³nia samobójstwo kobiety czy mê¿czyŸni?
names(dane)[2:4]=c("ogó³em","mê¿czyŸni","kobiety")

czestosc_kobiety=dane[1,]$kobiety/dane[1,]$ogó³em*100
czestosc_kobiety
czestosc_mezczyzni = 100-czestosc_kobiety
czestosc_mezczyzni

plec = data.frame(
  p³eæ=c("kobieta", "mê¿czyzna"),
  value=c(czestosc_kobiety, czestosc_mezczyzni))

ggplot(plec, aes(x="", y=value, fill=p³eæ)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title="Próby samobójcze z podzia³em na p³eæ")

#5 Kto skuteczniej pope³nia samobójstwa, kobiety czy mê¿czyŸni?
names(dane)[8:9]=c("zgon mê¿czyŸni", "zgon kobiety")

skutecznosc=data.frame(name=c("kobiety","mê¿czyŸni"), 
                       val=c(dane[1,]$`zgon kobiety`/dane[1,]$kobiety*100,
                         dane[1,]$`zgon mê¿czyŸni`/dane[1,]$mê¿czyŸni*100))

skutecznosc %>%
  ggplot(aes(x=name, y=val)) +
  geom_segment(aes(xend=name, yend=0))+
  geom_point(size=4, color="orange") +
  labs(x="p³eæ", y="skutecznoœæ[%]", title="Skutecznoœæ samobójstw wsród mê¿czyzn i kobiet")+
  ylim(0,100)


#6 Odsetek samobójstw w danym województwie
dane%>%
  select(Województwo, ogó³em, liczba.ludnoœci)%>%
  mutate(smiertelnosc=ogó³em/liczba.ludnoœci*100)%>%
  filter(Województwo!="POLSKA")%>%
  ggplot(aes(x=smiertelnosc, y=reorder(Województwo, smiertelnosc)))+
  geom_point(size=4, col="red")+
  labs(y="województwo", x="œmiertelnoœæ[%]", title="Procent samobójstw w danym województwie")+
  gghighlight(Województwo == "MA£OPOLSKIE")+
  xlim(0,0.075)


         