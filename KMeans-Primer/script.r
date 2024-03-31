## Napomena: ova skripta sadrzi originalno resenje jednog od studenata. 
## Na nekoliko mesta u skripti su dodate napomene kako bi se ukazalo na neka malo bolja resenja
## ili na blagu izmenu nekih formulacija kako bi se iste ucinile jasnijim      

# ucitavamo pocetni dataset
df <- read.csv("spotify.csv", stringsAsFactors = F)

# izbacujemo iz skupa podataka one obzervacije koje imaju vrednost atributa speechiness > 60
indeksi <- which(df$speechiness > 60)
df <- df[-indeksi,]

sum(df$speechiness > 60)
# vidimo da sada imamo 0 obzervacija koje imaju vrednost atributa speechiness > 60

# pre nego sto odredimo znacaj atributa, malo cemo da sredimo skup podataka
# kako kmeans algoritam radi sa numerickim podacima na ulazu, kolonu track_name cemo izbaciti iz daljeg
# razmatranja jer ne mozemo da je pretvorimo u numericku varijablu a da sa logicke strane donese neku vaznu informaciju
df$track_name <- NULL
table(df$mode)
# kako je raspodela promenljive mode slicna za obe vrednosti, a nemamo informaciju o tome kakav je odnos izmedju
# tih vrednosti kako bi ih pretvorili u numericku varijablu, izbacicemo i atribut mode

#Dodatna napomena: varijablu mode smo mogli i da prepoznamo kao ordinalnu pa da je na taj nacin posmatramo.
df$mode <- NULL

# proveravamo postojanje nedostajucih vrednosti
apply(df, 2, FUN = function(x) sum(is.na(x) | x == "" | x == " " | x == "-"))
# kolona in_spotify_charts se pretvara u numericku kolonu prvo pa ce se onda zameniti njene vrednosti odgovarajucom
# vrednoscu
df$in_spotify_charts <- as.numeric(df$in_spotify_charts)
summary(df$in_spotify_charts)
# da bi zamenili na vrednosti prvo proveravamo raspodelu promenljive
shapiro.test(df$in_spotify_charts)
# kako promenljiva dolazi iz raspodele cija distribucija nije normalna, na vrednosti cemo zamentiti medijanom
df$in_spotify_charts[is.na(df$in_spotify_charts)] <- median(df$in_spotify_charts , na.rm = T)
summary(df$in_spotify_charts)
# vidimo da vise nemamo na vrednosti
# isti postupak radimo i za promemnljivu in_apple_charts
shapiro.test(df$in_apple_charts)
# na vrednosti kod in_apple_charts kolone menjamo takodje sa medijanom
df$in_apple_charts[is.na(df$in_apple_charts)] <- median(df$in_apple_charts, na.rm = T) 
summary(df$in_spotify_charts)
# izbacili smo na vrednosti iz kolone in_apple_charts 
apply(df, 2, FUN = function(x) sum(is.na(x) | x == "" | x == " " | x == "-"))
# jos jednom proveravamo prisustvo na vrednosti i vidimo da nemamo vise nijednu


# sada cemo odraditi znacajnost varijabli kreiranjem matrice korelacije
library(ggplot2)
cm <- cor(df)
cm
library(corrplot)
corrplot.mixed(cm,  tl.cex = 0.7, number.cex = 0.7)
# vidimo da atributi kao sto su streams i in_apple_playlist su visoko korelisani sa sa nekim drugim atributima
# tako da cemo njih izbaciti iz daljeg razmatranja
df$streams <- NULL
df$in_apple_playlists <- NULL 
# ostale atribute za sada zadrzavamo u daljoj analizi

# pre daljem rada, potrebno je proveriti postojanje autlajera (ekstremnih vrdnosti) u skupu podataka
# zbog nacina rada kmeans algoritma, autlajeri mogu znacajno uticati na dobijene klastere, te je njih potrebno
# obraditi na odgovoarajuci nacin
# proveravamo postojanje autlajera
apply(df, 2, FUN = function(x) length(boxplot.stats(x)$out))
# za kolone koje imaju ukupan broj autlajera veci od 10% od ukupnog broja obzervacija smo se odlucili
# da izbacimo iz daljeg razmatranja jer primenom tehnika za otklanjanje autlajera cemo znacajno uticati
# na raspodelu tih atributa te smatramo da takve kolone treba izbaciti iz daljeg razmatranja
df$released_year <- NULL
df$in_spotify_playlists <- NULL
df$speechiness <- NULL

# za ostale kolone, koje imaju manje od 10% autlajera, cemo primeniti tehniku Winsorize
library(DescTools)
# za svaki od preostalih atributa cemo iscrtati boxplot, videti sa koje strane se nalaze autlajeri
# i zameniti ih odgovarajucim vrednostima (najceste 5 i 95 percentilom u zavisnosti od vrednosti autlajera)
boxplot(df$in_spotify_charts)
tmp <- Winsorize(df$in_spotify_charts, probs = c(0, 0.95))
boxplot(tmp)
# pomericemo jos nize granicu jer idalje postoje autlajeri
tmp <- Winsorize(df$in_spotify_charts, probs = c(0, 0.92))
boxplot(tmp)
# sada cemo ovakve vrednosti dodeliti koloni in_spotify_charts i ovaj postupak primeniti za sve ostale kolone
df$in_spotify_charts <- tmp
boxplot(df$in_spotify_charts)

boxplot(df$in_apple_charts)
tmp <- Winsorize(df$in_apple_charts, probs = c(0, 0.95))
boxplot(tmp)
df$in_apple_charts <- tmp
boxplot(df$in_apple_charts)

boxplot(df$bpm)
tmp <- Winsorize(df$in_apple_charts, probs = c(0, 0.95))
boxplot(tmp)
df$bpm <- tmp
boxplot(df$bpm)

boxplot(df$energy)
tmp <- Winsorize(df$energy  , probs = c(0.05, 1))
boxplot(tmp)
df$energy   <- tmp
boxplot(df$energy)

boxplot(df$instrumentalness)
table(df$instrumentalness)
tmp <- Winsorize(df$instrumentalness  , probs = c(0, 0.95))
boxplot(tmp)
# uzimamo jos manju vrednost jer idalje ima autlajera
tmp <- Winsorize(df$instrumentalness  , probs = c(0, 0.9))
boxplot(tmp)
df$instrumentalness   <- tmp
boxplot(df$instrumentalness)
table(df$instrumentalness)
# kod atributa instrumentalness vidimo jedno vrlo zanimljivu stvar a to je da nakon uklanjanja autlajera
# sve obezrvacije imaju vrednost 0 za dati atribut
# to znaci da taj atribut ne nosi nikakvu informaciju i zato cemo ga izbaciti iz daljeg razmatranja
df$instrumentalness <- NULL

boxplot(df$liveness)
tmp <- Winsorize(df$liveness  , probs = c(0, 0.95))
boxplot(tmp)
df$liveness   <- tmp
boxplot(df$liveness)


apply(df, 2, FUN = function(x) length(boxplot.stats(x)$out))
# ponovo izvrsavamo pocetnu funkciju i vidimo da vise nemamo autlajere

summary(df)
# sada cemo odraditi proces normalizacije kako bi sveli sve vrednosti na isti opseg
# a u cilju da zbog razlicitih velicina atributa neki atributi ne bi postali znacajniji samo
# zato sto imaju veci opseg
normalizuj <- function(x) {
  if(all(is.na(x) | x == 0)){
    return (x)
  }
  if (max(x) == min(x)) {
    return (x)
  }
  
  ( (x - min(x)) / (max(x) - min(x)) )
}

df2 <- as.data.frame(
  apply(df, 2, FUN = function(x) normalizuj(x))
)
summary(df2)
# sada ovako normalizovane vrednosti (vrednosti svih atributa svedeni na opseg od 0 do 1
# gde je 0 minimalna a 1 maksimalna vrednost) vracamo u pocetni skup podataka
df <- df2
summary(df)

# sada sprovodimo elbow metodu kako bi nasli najbolju vrednost za broj klastera
# prvo kreiramo pomocni dataframe u koje cemo smestati razlicite razultate za razlicite vrednosti klastera
df.tmp <- data.frame()
# odradicemo proveru za opseg od 2 do 10 klastera i videti koja vrednost od tih daje najbolje rezultate
for (k in 2:10) {
  model <- kmeans(x = df, 
                  centers = k,
                  iter.max = 20, 
                  nstart = 1000)
  df.tmp <- rbind(df.tmp,
                  c(k, model$tot.withinss, model$betweenss / model$totss))
}
df.tmp
# radi bolje preglednosti promenicemo nazive kolona
colnames(df.tmp)<- c("k", "tot.withinss", "ratio")
df.tmp
# iscrtacemo grafik da zagledamo ponasanje modela za razlicite vrednosti parametra k
library(ggplot2)
ggplot(data = df.tmp, aes (x = k, y = tot.withinss)) + geom_line() + geom_point()
# sagledavajuci grafik kao i vrednosti koje se nalaze u df.tmp setu vidimo da su nam ponajbolje opcije za broj klastera 2 ili 3,
# s tim da za nijansu su bolje vrednosti za broj klastera = 2 te cemo tu vrednost uzeti za kreiranje model

# izvrsavamo klasterizaciju za broj klastera = 2
model <- kmeans(x = df, 
                centers = 2,
                iter.max = 20, 
                nstart = 1000)
model
# vidimo da sve obzervacije se nalaze ili u klasteru 1 ili u klasteru 2

source("Utility.R")
stats <- summary.stats(df, model$cluster, 2)
stats
# ucitana je skripta Utility i iz nje pozvana funkcija summary.stats koja nam daje odredjene statistike za model koji smo
# kreirali a na osnovu kojih cemo izvrsiti interpretaciju dobijenih klastera
# 1. Broj pesama po klasteru
# U klasteru 1 imamo 378 pesama, dok u klasteru 2 imamo 574, odnosno klaster 2 sadrzi vise pesama

# 2. Centri klastera
# U klasteru 1 se uglavnom nalaze pesme koje su visoko kotirane na applerang listi (in_apple_charts) i imaju veci bpm,
# odnosno brze su pesme. Sa druge strane, klaster 2 uglavnom sadrzi pesme nisko kotirane na applerang listi, koje
# imaju nesto sporiji ritam (bpm) i takodje nisu dobro kotirane na spotify rang listi

# 3. Disperzija od centara
# Za klaster 2 mozemo da kazemo da, iako je veci po svom obimu, je homogeniji jer je disperzija od centra klastera 
# za skoro sve atribute manja u odnosu na model 1 a noricto atribute in_apple_charts i bpm koji su se pokazali kao
# znacajni za ovako kreiran model. Kod klastera 1, kojeg karakterise manja homogenost u pogledu na klaster 2,
# vidimo da za atribut in_spotify_charts imamo najvecu disperziju obzervacija u odnosu na centar klastera, sto bi znacilo
# da se u tom klasteru mogu naci i pesme koje imaju visoku poziciju na sporitfy rang listi ali i nisku poziciju


## Napomena: neke od prethodnih formulacija su blago izmenjene, bez promene znacenja, kako bi se ucinile jasnijim i/ili 
## kompletnijim. VAZNO: date interpretacije tj nacin na koji su iskazane nije jedini korektan nacin, vec pokazuje samo 
## jedan od nacina da se formulisu interpretacije metrika kako bi se odgovorilo na zahtev zadatka. 
## 