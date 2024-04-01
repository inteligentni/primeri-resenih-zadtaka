# ucitavanje dataseta
data <- read.csv("apples.csv", stringsAsFactors = F)

# upoznajemo se sa strukturom dataseta
str(data)

# provera da li varijabla od koje cemo kreirati izlaznu sadrzi NA vrednosti
all(!is.na(data$Quality))

# kreiranje izlazne varijable
data$IsGood <- ifelse(data$Quality == "good", "yes", "no")
data$IsGood <- as.factor(data$IsGood)
data$Quality <- NULL

# izbacujemo varijable koje nema smisla iskoristiti za predvidjanje kvaliteta jabuke
data$A_id <- NULL
data$Code <- NULL

# Varijabla Continent ima 6 mogucih vrednosti, tako da je pretvaramo u faktorsku
length(unique(data$Continent))
data$Continent <- as.factor(data$Continent)
# naredne varijable su u osnovi numericki podaci tako da ih pretvaramo
data$Weight <- as.numeric(data$Weight)
data$Sweetness <- as.numeric(data$Sweetness)
data$Acidity <- as.numeric(data$Acidity)
 
# provera nedostajucih vrednosti
apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# uocavamo nedostajuce vrednosti kod varijabli Weight, Sweetness, Acidity

apply(data, MARGIN = 2, FUN = function(x) sum(x == "", na.rm=T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "-", na.rm=T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == " ", na.rm=T))
# nema nedostajucih vrednosti u nekom drugom obliku

# nije neophodno proveravati da li varijable imaju normalnu raspodelu jer ako imaju svakako je medijana jednaka mean-u
# tako da sve NA vrednosti menjamo medijanom
medianW<-median(data$Weight, na.rm=T)
data$Weight[is.na(data$Weight)] <- medianW 

medianS<-median(data$Sweetness, na.rm=T)
data$Sweetness[is.na(data$Sweetness)] <- medianS 

medianA <- median(data$Acidity, na.rm=T)
data$Acidity[is.na(data$Acidity)] <- medianA 

# vise nemamo nedostajuce vrednosti
apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# kako bismo izvrsili odabir atributa vizualizovacemo podatke
library(ggplot2)

# na grafiku vidimo male razlike u pogledu izlazne varijable u odnosu na Continent
# tako da cemo ovu varijablu koristiti za model
ggplot(data, aes(x = Continent, fill=IsGood)) +
  geom_bar(position = "dodge", width = 0.4)

# na grafiku vidimo da se vrednosti za Size razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za model
ggplot(data, aes(x = Size, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Weight razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za model
ggplot(data, aes(x = Weight, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Sweetness razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za model
ggplot(data, aes(x = Sweetness, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Crunchiness razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za mode
ggplot(data, aes(x = Crunchiness, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Juiciness razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za mode
ggplot(data, aes(x = Juiciness, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Ripeness razlikuju u pogledu izlazne varijable 
# tako da cemo ovu varijablu koristiti za mode
ggplot(data, aes(x = Ripeness, fill=IsGood)) +
  geom_density(alpha = 0.5)

# na grafiku vidimo da se vrednosti za Acidity ne razlikuju puno u pogledu izlazne varijable
# tako da ovu varijablu necemo koristiti za kreiranje naseg modela
ggplot(data, aes(x = Acidity, fill=IsGood)) +
  geom_density(alpha = 0.5)
data$Acidity <- NULL

# obzirom na to da algoritam Naive Bayes radi sa faktorskim i numerickim varijablama ako one imaju normalnu raspodelu
# a ako nemaju potrebno je da se izvrsi diskretizacija
# moramo prvo da izvrsimo proveru normalnosti raspodele za numericke varijable
num.vars <- c(2:7)

apply(data[,num.vars],
      MARGIN = 2,
      FUN = function(x) shapiro.test(x))

# Na osnovu shapiro testa zakljucujemo da:
# Varijable Size, Juiciness, Ripeness imaju normalnu raspodelu
# Varijable Weight, Sweetness, Crunchiness nemaju normalnu raspodelu

# Varijable koje imaju normalnu raspodelu mozemo da iskoristimo za kreiranje modela
# Varijable koje nemaju normalnu raspodelu moramo prvo da diskretizujemo

to_discretize <- c(3,4,5)

# pokusacemo da podelimo ukupan opseg vrednosti svake varijable na 5 intervala jednake ucestalosti (kvantilna diskretizacija)
library(bnlearn)
discretized <- discretize(data[,to_discretize],
                          method = 'quantile',
                          breaks = c(5,5,5))

# vidimo da je diskretizacija uspesno izvrsena
summary(discretized)

data.new <- cbind(data[,c(1,2)], discretized, data[,c(6,7,8)])

# delimo dataset na trening (80% pocetnog dataseta) i test (20% pocetnog dataseta)
library(caret)
set.seed(10)
train_indices <- createDataPartition(data.new$IsGood, p = 0.8, list = FALSE)
train.data <- data.new[train_indices,]
test.data <- data.new[-train_indices,]

library(e1071)
# kreiramo model
nb1 <- naiveBayes(IsGood ~ ., data = train.data)

# vrsimo predvidjanje
nb1.pred <- predict(nb1, newdata = test.data, type = "class")

#kreiramo matricu konfuzije
nb1.cm <- table(true = test.data$IsGood, predicted = nb1.pred)
nb1.cm

# interpretacija matrice konfuzije
# True negative (TN) - za 207 jabuka smo predvideli da nece biti dobrog kvaliteta i one zaista nisu dobrog kvaliteta
# True positive (TP) - za 232 jabuke smo predvideli da ce biti dobrog kvaliteta i one zaista jesu dobrog kvaliteta
# False positive (FP) - za 92 jabuke smo predvideli da ce biti dobrog kvaliteta a one zapravo nisu dobrog kvaliteta
# False negative (FN) - za 68 jabuka smo predvideli da nece biti biti dobrog kvaliteta a one zapravo jesu dobrog kvaliteta

# definisemo funkciju za izracunavanje evalucionih metrika
compute.eval.metrics <- function(cmatrix) {
  
  TP <- cmatrix[2,2] 
  TN <- cmatrix[1,1] 
  FP <- cmatrix[1,2] 
  FN <- cmatrix[2,1]
  
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

#4 metrike koje se koristimo za evaluaciju performansi klasifikacionog modela su:
#Accuracy (tacnost) - procenat tacnih predikcija (i pozitivnih i negativnih). 
#Precision (preciznost) - procenat tacno predvidjenih pozitivnih predikcija u odnosu na sve pozitivne predikcije koje je model napravio
#Recall (odziv) - procenat pozitivnih predikcija koje je model tacno identifikovao kao pozitivne u odnosu na ukupan broj stvarno pozitivnih
#F1 - mera za balansiranje vrednosti precision-a i recall-a

# racunamo ove metrike za nas model
nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval

# Tumacimo metrike u nasem domenu problema: 

# Accuracy - za 73,2% jabuka smo tacno predvideli da li ce ostvariti dobar kvalitet ili ne
# Precision - od svih jabuka za koje smo predvideli da ce biti dobrog kvaliteta, 71,6% je stvarno dobrog kvaliteta
# Recall - od svih jabuka koje su zaista dobrog kvaliteta, mi smo tacno predvideli 77,3% njih
# F1 - mera koja balansira preciznost i odziv i kod nas iznosi 0.74

# ROC kriva

# kreiramo predikcije u formi verovatnoca
nb2.pred.prob <- predict(nb1, newdata = test.data, type = "raw")
nb2.pred.prob

library(pROC)
# kreiramo roc krivu
nb2.roc <- roc(response = as.numeric(test.data$IsGood), 
               predictor = nb2.pred.prob[,2])

nb2.roc$auc
# AUC je povrsina ispod ROC krive i ona nam govori koliko dobro model razdvaja pozitivnu od negativne klase, kod nas iznosi 0.81

nb2.coords <- coords(nb2.roc, 
                     ret = c("spec", "sens", "thr"),
                     x = "local maximas", transpose = FALSE)
nb2.coords

# koristimo youden metodu koja ce nam odrediti prag verovatnoce kojim se maksimizuje suma sensitivity i specificity metrika
plot.roc(nb2.roc, 
         print.thres = TRUE, 
         print.thres.best.method = "youden")

prob.threshold <- 0.451

# vrsimo ponovo predvidjanje sa novim pragom verovatnoce
nb2.pred2 <- ifelse(test = nb2.pred.prob[,2] >= prob.threshold, 
                    yes = "Yes", 
                    no = "No")
nb2.pred2 <- as.factor(nb2.pred2)

# kreiramo novu matricu konfuzije
nb2.cm2 <- table(actual = test.data$IsGood, predicted = nb2.pred2)
nb2.cm2

# ponovo racunamo evalucione metrike
nb2.eval2 <- compute.eval.metrics(nb2.cm2)

data.frame(rbind(nb1.eval, nb2.eval2), row.names = c("nb1", "nb2"))
# kada uporedimo metrike, mozemo da zakljucimo da je tacnost ostala ista
# preciznost se vrlo malo smanjila 
# odziv se povecao, samim tim i metrika F1