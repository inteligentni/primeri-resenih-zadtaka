#Učitavanje dataseta
data <- read.csv("fastfood.csv", stringsAsFactors = FALSE)
summary(data)
str(data)

#KREIRANJE PODSKUPA PODATAKA koji sadrži proizvode čije ukupne masti su <= 125
data.subset <- subset(data, data$total_fat <= 125)

#PROVERA NEDOSTAJUĆIH VREDNOSTI (NA,“-”, ” ”, ””)
apply(data.subset, MARGIN = 2, FUN = function(x) length(which(is.na(x))))
#Atributi cal_fat(1), fiber(12), protein(2) i calcium(209) imaju nedostajuće(NA) vrednosti.

apply(data.subset, MARGIN = 2, FUN = function(x) length(which(x == "-" | x == " " | x == "")))
#Atribut restaurant(3) ima nedostajuće vrednosti.

#ZAMENA NEDOSTAJUĆIH VREDNOSTI
column.with.na<-c(4,11,13,14)
apply(data.subset[,column.with.na],MARGIN = 2, FUN = function(x) shapiro.test(x))
#Svi atributi koji imaju NA vrednosti nemaju normalnu raspodelu. 
#S obzirom da atributi nemaju normalnu raspodelu (p-value<0.05), menjamo nedostajuće vrednosti medijanom.

cal_fat_median <- median(data.subset$cal_fat, TRUE)
data.subset$cal_fat[is.na(data.subset$cal_fat)] <- cal_fat_median

fiber_median <- median(data.subset$fiber, TRUE)
data.subset$fiber[is.na(data.subset$fiber)] <- fiber_median

protein_median <- median(data.subset$protein, TRUE)
data.subset$protein[is.na(data.subset$protein)] <- protein_median

#Atribut calcium ima dosta nedostajućih vrednosti, pa neće biti ni zamenjene.
#Atribut calcium se izbacuje iz dataseta, zbog prevelikog broja nedostajućih vrednosti.
data.subset$calcium <- NULL

#Zamena - i " " sa NA
data.subset$restaurant[data.subset$restaurant == "-" | data.subset$restaurant == " " | data.subset$restaurant == ""] <- NA
table(data.subset$restaurant)
#S obzirom da je restaurant character varijabla, gledamo koja vrednost je najzastupljenija među podacima.
#Za atribut restaurant najučestalija je vrednost "Taco Bell", pa sa njom manja NA vrednosti.
data.subset$restaurant[is.na(data.subset$restaurant)] <- "Taco Bell"


#IZBOR ATRIBUTA ZA MODEL LINEARNE REGRESIJE

#Model LR podržava samo numeričke atribute.
#Naziv restorana (atribut restaurant) i naziv proizvoda (atribut item) se neće koristiti za kreiranje modela, 
#   jer su one character varijable koje se ne mogu pretvoriti u numeričke. 
data.subset$restaurant <- NULL
data.subset$item <- NULL

#Značajnost ostalih atributa se ispituje pomoću korelacione matrice
cor.mat <- cor(data.subset) 
library(corrplot)
corrplot.mixed(cor.mat, tl.cex = 0.75, number.cex = 0.75)
#Na korelacionoj matrici se može primetiti da su za predviđaje atributa protein najznačajni(najveća korelisanost) atributi:
#   1)cholesterol - 0.87 (jedinično povećanje atributa cholesterol dovešće do povećanja atributa protein za 0.87)
#   2)calories - 0.8 (jedinično povećanje atributa calories dovešće do povećanja atributa protein za 0.8)
#   3)sodium - 0.76 (jedinično povećanje atributa sodium dovešće do povećanja atributa protein za 0.76)

#Zaključak: Za predviđanje atributa protein (prvi model LR) koristiće se atributi cholesterol,calories i sodium.


#KREIRANJE MODELA LR
#Podela dataseta na train i test
#Train set sadrzi 80% opservacija,a test ostalih 20%
library(caret)
set.seed(123) 
i <- createDataPartition(data.subset$protein, p = 0.8, list = FALSE)
train <- data.subset[i, ]
test <- data.subset[-i, ]

#Kreiranje modela LR
lm1 <- lm(protein ~ cholesterol + calories + sodium, data = train) 
summary(lm1)

comment("-Residuali predstavljaju razliku između stvarnih vrednosti zavisne varijable(protein) i 
            predviđenih vrednosti koje model generiše. Vrednost reziduala bi trebala da bude jednaka nuli.
            Medijana reziduala je -0.702. To znači da je polovina naših podataka imala rezidualne vrednosti 
            manje od -0.702, a druga polovina veće.
         -Intercept je 2.4256456, što znači da kada su svi ostali atributi 0, očekujemo da je prosečna
            vrednost atributa protein 2.4256456.
         -Koeficijenti za varijable cholesterol i sodium su statistički značajni jer su p-vrednosti daleko manje od 0.05,
            dok koeficijent za calories nije statistički značajan.
            Dakle cholesterol i sodium su relevantne za predikciju nivoa proteina u modelu.
         -Koeficijenti: Kada se ostale varijable drže konstantnim:
            Jedinično povećanje cholesterol za 1 dovešće do povećanja protein za 0.182 (ovo znači da postoji pozitivna linearna veza između nivoa holesterola i proteina, tj. što je veći nivo holesterola, veći je i nivo proteina).
            Jedinično povećanje calories za 1 dovešće do povećanja protein za 0.0034,
              međutim s obzirom da calories nije statistički značajan, ne možemo da kažemo da će značajano uticati na nivo proteina).
            Jedinično povećanje sodium za 1 dovešće do povećanja protein za 0.0083 (postoji pozitivna linearna veza između nivoa sodium-a i proteina).
         -Standardna greška reziduala je 6.492, što nam daje informaciju o prosečnoj udaljenosti između stvarnih vrednosti i predviđenih vrednosti.
         -Koeficijent determinacije (R-kvadrat):Model objašnjava oko 83.52% varijabiliteta protein varijable.
        -F-statistika: Nulta hipoteza F-statistike glasi da svi koeficijenti nezavisnih varijabli su nula, 
            što znači da nezavisne varijable ne utiču na zavisnu varijablu. 
            S obzirom da je p-vrednost manja od 0.05, može se zaključiti da model ima značajnu prediktivnu moć i da ga treba razmatrati.")

lm1.pred <- predict(lm1, test)
lm1.pred

# DIJAGNOSTIČKI GRAFIKONI
par(mfrow = c(2,2)) 
plot(lm1) 
par(mfrow = c(1,1)) 

comment("-Reziduals vs Fitted- prikazuje raspodelu reziduala (razlike između stvarnih i predviđenih vrednosti) u odnosu na predviđene vrednosti (da li je zadovoljenja pretpostavka lineranosti). 
            Residuali bi trebali da budu raspoređeni oko horizontalne linije (njihova vrednost treba da bude 0).                    
            Sa grafikona može da se zaključi da postoje manja odstupanja, tj. nelineranost u podacima, što bi moglo dovesti do neadekvatnosti modela.
         -Normal Q-Q- govori o normalnoj raspodeli reziduala. Tačke bi trebale da se prate duž dijagonale.
            S obzirom da tačke odstupaju od dijagonale na krajevima, ne možemo se u potpunosti osloniti na model.
         -Scale-Location- prikazuje da li varijabilitet(greške,odstupanja) reziduala variraju na ujednačen način. 
            Crvena linija treba da bude horizontalna. Na grafikonu varijansa reziduala nije konstantna, što može dovesti do neadekvatnosti modela.
         -Residuals vs. Leverage-prikazuje ekstremno visoke i/ili ekstremno niske vrednosti.
            Većina tačaka trebala bi biti unutar granica, a ekstremne vrednosti bi trebale biti retke (nije dobro ukoliko postoje).
            Na osnovu Kukova distance, možemo zaključiti da model sadrži outlayer.")


#MULTIKOLINERANOST
library(car)
vif(lm1)
sqrt(vif(lm1))
#S obzirom da je za atribut calories sqrt(vif) vrednost >2, može se zaključiti da postoji multikorelisanost, koja može dovesti do nepouzdanih procena.
#Novi model za predikciju proteina treba kreirati sa atributima cholesterol i sodium. 

lm2<-lm(protein ~ cholesterol+ sodium, data = train) 
sqrt(vif(lm2))
#Multikolienearnost ne postoji više.
summary(lm2)

