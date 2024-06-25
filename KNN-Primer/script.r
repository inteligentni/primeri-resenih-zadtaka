## Napomena: ova skripta sadrzi originalno resenje jednog od studenata. 
## Na nekoliko mesta u skripti su dodate napomene kako bi se ukazalo na neka malo bolja resenja
## ili na blagu izmenu nekih formulacija kako bi se iste ucinile jasnijim      

#ucitavamo dataset
data <- read.csv("spotify.csv", stringsAsFactors = F)
str(data)


#proveravamo da li varijabla streams ima nedostajuce vrednosti jer na osnovu nje kreiramo izlaznu promenljivu
sum(is.na(data$streams))
sum(data$streams == "", na.rm = T)
sum(data$streams == " ", na.rm = T)
sum(data$streams == "-", na.rm = T)

#nema nedostajucih vrednosti, racunamo treci kvartil
treciKvartil <- quantile(data$streams, 0.75)
treciKvartil

data$High_Streams <- ifelse(data$streams > treciKvartil, yes = "yes", no = "no")
data$High_Streams <- as.factor(data$High_Streams)

#odmah izbacujemo varijablu streams jer smo je koristili za kreiranje izlazne varijable, pa nam nece biti vise potrebna za model
data$streams <- NULL
str(data)

#IZBOR VARIJABLI - KNN koristi samo numericke podatke!

#proveravamo koje cemo varijable ukljuciti u model, proveravamo koliko imaju razlicitih vrednosti
length(unique(data$track_name))
#svaka pesma ima jedinstveno ime i nece uticati na to da li pesma ima veliki broj pustanja pesme
data$track_name <- NULL

length(unique(data$in_spotify_charts))
table(data$in_spotify_charts)
#mozemo je prebaciti u numericku varijablu (jer je to sustinski numericka vrednost), necemo je izbacivati

length(unique(data$mode))
#takodje se moze pretvoriti u numercku s obzirom da je ordinalna

#proveravamo da li promenljive imaju nedostajuce vrednosti
apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == " ", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "-", na.rm = T))
#in_spotify_charts i in_apple_charts imaju 1 i 2 NA vrednosti respektivno
#in_spotify_charts ima "-" vrednost

data$in_spotify_charts[data$in_spotify_charts == "-"] <- NA
data$in_spotify_charts <- as.numeric(data$in_spotify_charts, na.rm = T)
#proveravamo da li ima normalnu raspodelu kako bismo znali sa kojim vrednostima menjamo NA vrednosti i za in_spotify_charts i za in_apple_charts
apply(X = data[,c(3,5)], 2, FUN = function(x) shapiro.test(x))
#obe imaju vrednost p < 0.05 sto znaci da nemaju normalnu raspodelu i NA vrednosti menjamo medijanom
medijanaSpotify <- median(data$in_spotify_charts, na.rm = T)
medijanaSpotify
medijanaApple <- median(data$in_apple_charts, na.rm = T)
medijanaApple
#dobili smo da su medijane 3 i 38 za spotify i apple respektivno
data$in_spotify_charts[is.na(data$in_spotify_charts)] <- medijanaSpotify
data$in_apple_charts[is.na(data$in_apple_charts)] <- medijanaApple

#nema vise nedostajucih vrednosti
str(data)
data$mode <- as.factor(data$mode)

#radimo plotovanje kako bismo jos jednom proverili koje cemo varijable ukljuciti u nas model
library(ggplot2)
ggplot(data, mapping = aes(x = released_year, fill = High_Streams)) + geom_density(alpha = 0.5)
#sto je skorija godina izdanja to je manja verovatnoca da pesma ima visok broj pustanja
ggplot(data, mapping = aes(x = in_spotify_charts, fill = High_Streams)) + geom_density(alpha = 0.5)
#sto je veca pozicija pesme na spotify rang listi to je veca verovatnoca da ima veci broj pustanja, ta verovatnoca opada sa smanjenjem pozicije
ggplot(data, mapping = aes(x = in_spotify_playlists, fill = High_Streams)) + geom_density(alpha = 0.5)
#ukoliko se pesma nalazi u malom broju plejlista na spotify platformi to je manja verovatnoca da ima veci broj pustanja
ggplot(data, mapping = aes(x = in_apple_playlists, fill = High_Streams)) + geom_density(alpha = 0.5)
#ukoliko se pesma nalazi u malom broju plejlista na apple platformi to je manja verovatnoca da ima veci broj pustanja
ggplot(data, mapping = aes(x = in_apple_charts, fill = High_Streams)) + geom_density(alpha = 0.5)
#ukoliko pesma nema poziciju na apple rang listi to je veca verovatnoca da nema veliki broj pustanja
ggplot(data, mapping = aes(x = bpm, fill = High_Streams)) + geom_density(alpha = 0.5)
#nema neke prevelike razlike u broju pustanja pesme kada je u pitanju mera brzine pesme, nije nam relevantna za model
data$bpm <- NULL
ggplot(data, mapping = aes(x = liveness, fill = High_Streams)) + geom_density(alpha = 0.5)
#isto vazi i za nivo prisustva elemenata uzivo izvodjenja, ni ona nije relevantna za nas model
data$liveness <- NULL
ggplot(data, mapping = aes(x = energy, fill = High_Streams)) + geom_density(alpha = 0.5)
#ni energetski nivo pesme ne utice na broj pustanja pesme, mozemo je izbaciti
data$energy <- NULL
ggplot(data, mapping = aes(x = instrumentalness, fill = High_Streams)) + geom_density(alpha = 0.5)
#ni instrumentalness nam nije relevantna i nju izbacujemo
data$instrumentalness <- NULL
ggplot(data, mapping = aes(x = speechiness, fill = High_Streams)) + geom_density(alpha = 0.5)
#ni ova varijabla nam ne utice na model tako da cemo je izbaciti
data$speechiness <- NULL
ggplot(data, mapping = aes(x = mode, fill = High_Streams)) + geom_bar(position = 'dodge')
#veca je verovatnoca da pesma ima veci broj pustanja ako je karakter pesme major

str(data)
data$mode <- as.numeric(data$mode)
#prebacili smo varijablu mode u numericku jer knn model radi samo sa numerickim varijablama
#gotovo je sredjivanje podataka


#proveravamo da li neke variujable imaju autlajere

apply(X = data[,c(1,2,3,4,5,6)], 2, FUN = function(x) length(boxplot.stats(x)$out))
#vidimo da sve varijable imaju autlajere, potrebno je da ih standardizujemo
#proveravamo da li imaju normalnu raspodelu kako bismo znali na koji nacin cemo izvrsiti standardizaciju
apply(X = data[,c(1,2,3,4,5,6)], 2, FUN = function(x) shapiro.test(x))
#sve varijable imaju p < 0.05 sto znaci da nemaju normalnu raspodelu

data.std <- apply(X = data[,c(1,2,3,4,5,6)], 2, FUN = function(x) scale(x, center = median(x), scale = IQR(x)))
data.std <- as.data.frame(data.std)

#kreirali smo standardizovani dataframe
#za scale smo stavili IQR - interquartile range sto predstavlja razliku izmedju 3. i 1. kvartila i kao centar smo stavili medijanu
#ubacujemo u standardizovani dataset preostalu varijablu mode i izlaznu varijablu
data.std$mode <- data$mode
data.std$High_Streams <- data$High_Streams

#sa tako standardizovanim dataset-om kreiramo nas model


#kreiramo train i test setove
library(caret)
set.seed(1010) # allows for repeating the randomization process exactly
indexes <- createDataPartition(data.std$High_Streams, p = 0.8, list = FALSE)
train.data <- data.std[indexes, ]
test.data <- data.std[-indexes, ]

#radimo krosvalidaciju sa 10 iteracija
library(e1071)
library(caret)
numFolds = trainControl(method = "cv", number = 10) 
kGrid =  expand.grid(.k = seq(from = 3, to = 25, by = 2))

#stavili smo neparne brojeve od 3 do 25 jer su nam oni potrebni kako bi jedna klasa bila dominantnija
knn.cv <- train( 
  x = train.data[, -7],
  y = train.data$High_Streams,
  method = "knn", # use rpart() to build multiple
  trControl = numFolds, # <folds> from above
  tuneGrid = kGrid) 
knn.cv
#dobili smo da nam je optimalna vrednost za parametar k = 7;
#mozemo je izuci direktno odatle 
kValue <- knn.cv$bestTune$k
kValue

library(class)
knn.pred <- knn(train = train.data[,-7], # training data without the output (class) variable
                test = test.data[,-7], # test data without the output (class) variable
                cl = train.data$High_Streams, # output (class) variable is specified here
                k = kValue) 
#kreirali smo model knn i sada mozemo kreirati matricu konfuzije
knn.cm <- table(true = test.data$High_Streams, predicted = knn.pred)
knn.cm

#       predicted
#  true   no yes
#  no    135   8
#  yes    12  35

#na glavnoj dijagonali matirce se nalaze tacne predikcije, a na sporednoj pogresne predikcije
#od ukupno 190 observacija tacno smo predvideli 170 sto znaci da ce nam accuracy biti veoma visok
#od ukupno 143 pesme koje stvarno nece imati veliki broj pustanja, tacno smo predvideli 135
#a od ukupno 47 pesama koje ce imati veliki broj pustanja, tacno smo predvideli 35

#TP - true positive - predstavlja sve pesme koje ce stvarno imati veliki broj pustanja a i mi smo predvideli da hoce
#TN - true negative - predstavlja sve pesme koje nece imati veliki broj pustanja i mi smo predvideli da nece
#FP - false positive - predstavlja sve pesme za koje smo rekli dace imati veliki broj pustanja a zapravo nemaju
#FN - false negative - predstavlja sve pesme za koje smo rekli da nece imati veliki broj pustanja a zapravo imaju


#kreiramo funkciju koja ce nam sluziti za evaluaciju modela
getEvaluationMetrics <- function(cm) {
  TP <- cm[2,2] 
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  F1 <- (2*precision*recall)/(precision + recall)
  
  c(Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = F1)
  
}
knn.eval <- getEvaluationMetrics(knn.cm)
knn.eval



#accuracy - predstavlja udeo tacno predvidjenih u odnosu na sve observacije.
#nas model je od ukupno 190 observacija tacno predvideo 170 i zbog toga nam je accuracy 89.47%
#za 170 pesama je tacno predvideo da li ce imati veliki broj pustanja pesme 

#precision - predstavlja udeo observacija za koje smo predvideli da su pozitivne i stvarno jesu
#za 43 pesme smo predvideli da ce imati veliki broj pustanja, od toga 35 pesama stvarno ima veliki broj pustanja
#i zbog toga nam je precision 81.39%

#recall - predstavlja udeo observacija koje su stvarno pozitivne a mi smo predvideli da jesu
#od ukupno 47 stvarno pozitivnih observacija mi smo tacno predvideli 35
#za 35 pesama smo rekli da ce imati veliki broj pustanja od ukupno 47 koje stvarno imaju veliki broj pustanja
#i zbog toga nam je recall 74.46%

#F1 - predstavlja meru performanski modela za balansirane vrednosti precision i recall, i za njom postoji potreba jer su
# precision i recall u antagonistickom odnosu, povecanje jedne, druga se smanjuje i obrnuto. Kada su i recall i precision visoke,
# visok je i F1.
# U kontekstu ovog zadatka, moze se reci da nas model dobro balansira između sposobnosti da prepozna većinu stvarno popularnih 
#pesama i toga da ne označava previše pesama kao veoma popularne kada nisu.

## Napomena: neke od prethodnih formulacija su blago izmenjene, bez promene znacenja, kako bi se ucinile jasnijim i/ili 
## kompletnijim. VAZNO: date interpretacije tj nacin na koji su iskazane nije jedini korektan nacin, vec pokazuje samo 
## jedan od nacina da se formulisu interpretacije metrika kako bi se odgovorilo na zahtev zadatka. 
