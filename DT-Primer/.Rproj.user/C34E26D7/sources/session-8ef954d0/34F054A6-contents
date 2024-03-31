## Napomena: ova skripta sadrzi originalno resenje jednog od studenata. 
## Na nekoliko mesta u skripti su dodate napomene kako bi se ukazalo na neka malo bolja resenja
## ili na blagu izmenu nekih formulacija kako bi se iste ucinile jasnijim      

# ucitavanje dataseta
data <- read.csv('bank.csv', stringsAsFactors = F)

str(data)

all(!is.na(data$Exited))

# pravljenje izlazne varijable
data$Stayed <- ifelse(data$Exited  == 0, 'Yes', 'No')
data$Stayed <- as.factor(data$Stayed)
data$Exited <- NULL

any(is.na(data$Age))

# dopuna nedostajucih vrednosti Age varijable
unique(data$Age)
data$Age[data$Age == "-"] <- NA
data$Age <- as.numeric(data$Age)
shapiro.test(data$Age)
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = T)

# pravljenje subseta koji cemo nadalje koristiti
dataSub <- subset(data, data$Age <= 87)
rm(data)

str(dataSub)

#provera nedostajucih vrednosti
apply(dataSub, 2, function(x) sum(is.na(x)))
apply(dataSub, 2, function(x) sum(x == "", na.rm = T))
apply(dataSub, 2, function(x) sum(x == "-", na.rm = T))
apply(dataSub, 2, function(x) sum(x == " ", na.rm = T))

# varijablu Surname necemo ukljuciti u model iz razloga sto je u pitanju karakter varijabla sa previse
# razlicitih vrednosti
length(unique(dataSub$Surname))
dataSub$Surname <- NULL

# varijabla Geography NA i - vrednosti tako da cemo ih zameniti dominantnom klasom i pretvoriti u faktorsku varijablu
length(unique(dataSub$Geography))
sort(table(dataSub$Geography))
dataSub$Geography[dataSub$Geography == "-" | is.na(dataSub$Geography)] <- 'France'
dataSub$Geography <- as.factor(dataSub$Geography)

str(dataSub)

# varijabla Gender ima samo dve moguce vrednosti tako da je pretvaramo u faktorsku 
length(unique(dataSub$Gender))
dataSub$Gender <- as.factor(dataSub$Gender)

# varijabla Card Type slicno Gender ima 4 moguce vrednosti tako da je pretvaramo u faktorsku
length(unique(dataSub$Card.Type))
dataSub$Card.Type <- as.factor(dataSub$Card.Type)

# varijable HasCrCard i IsActiveMember imaju samo dve moguce vrednosti tako da cemo ih takodje pretvoriti u faktorske
dataSub$HasCrCard <- factor(dataSub$HasCrCard, levels = 0:1, labels = c('No', 'Yes'))
dataSub$IsActiveMember <- factor(dataSub$IsActiveMember, levels = 0:1, labels = c('No', 'Yes'))

library(ggplot2)

# na grafiku vidimo da se vrednosti za CreditScore ne razlikuju mnogo u pogledu izlazne varijable tako da ovu
# varijablu necemo koristiti za model
ggplot(dataSub, aes(x = CreditScore, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()
dataSub$CreditScore <- NULL

# na grafiku vidimo da se vrednosti za Age razlikuju u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = Age, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()

# na grafiku vidimo da se vrednosti za Tenure ne razlikuju mnogo u pogledu izlazne varijable tako da ovu
# varijablu necemo koristiti za model
ggplot(dataSub, aes(x = Tenure, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()
dataSub$Tenure <- NULL

# na grafiku vidimo da se vrednosti za Balace ne razlikuju mnogo, ali dovoljno u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = Balance, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()

# na grafiku vidimo da se vrednosti za NumOfProducts razlikuju u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = NumOfProducts, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()

# na grafiku vidimo da se vrednosti za EstimatedSalary ne razlikuju mnogo u pogledu izlazne varijable tako da ovu
# varijablu necemo koristiti za model
ggplot(dataSub, aes(x = EstimatedSalary, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()
dataSub$EstimatedSalary <- NULL

# na grafiku vidimo da se vrednosti za Satisfaction.Score ne razlikuju znacajno u pogledu izlazne varijable tako da necemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = Satisfaction.Score, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()
## Napomena: Bolja opcija za vizuelizaciju je koriscenje bar plot-a, s obzirom da omogucuje bolje sagledavanje
## distribucije izlazne varijable za razlicite vrednosti ulazne varijable:
## ggplot(dataSub, aes(x = Satisfaction.Score, fill = Stayed)) + geom_bar(position = "fill") + theme_minimal()
dataSub$Satisfaction.Score <- NULL

# na grafiku vidimo da se vrednosti za Point.Earned ne razlikuju mnogo u pogledu izlazne varijable tako da ovu
# varijablu necemo koristiti za model
ggplot(dataSub, aes(x = Point.Earned, fill = Stayed)) + geom_density(alpha = 0.55) + theme_minimal()
dataSub$Point.Earned <- NULL

# na grafiku vidimo da se vrednosti za Geography razlikuju u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = Geography, fill = Stayed)) + geom_bar(position = 'dodge', width = 0.5) + theme_minimal()
## Napomena: Bolja opcija je koriscenje vrednosti "fill" za position parametar jer se time dobija prikaz u 
## relativnim vrednostima (proportions), a ne apsolutnim, pa se lakse moze sagledati znacajnost varijable
## Ne odnosi se samo na ovu varijablu, vec vazi generalno u slucaju koriscenja bar plota za proveru znacajnosti
## varijabli   

# na grafiku vidimo da se vrednosti za Gender razlikuju u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = Gender, fill = Stayed)) + geom_bar(position = 'dodge', width = 0.5) + theme_minimal()

# na grafiku vidimo da se vrednosti za HasCrCard ne razlikuju u pogledu izlazne varijable tako da necemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = HasCrCard, fill = Stayed)) + geom_bar(position = 'fill', width = 0.5) + theme_minimal()
dataSub$HasCrCard <- NULL

# na grafiku vidimo da se vrednosti za IsActiveMember razlikuju u pogledu izlazne varijable tako da cemo ovu
# varijablu koristiti za model
ggplot(dataSub, aes(x = IsActiveMember, fill = Stayed)) + geom_bar(position = 'dodge', width = 0.5) + theme_minimal()

# na grafiku vidimo da se proporcije za Card.Type ne razlikuju mnogo u pogledu izlazne varijable tako da ovu
# varijablu necemo koristiti za model
ggplot(dataSub, aes(x = Card.Type, fill = Stayed)) + geom_bar(position = 'fill', width = 0.5) + theme_minimal()
dataSub$Card.Type <- NULL

library(caret)
# delimo dataset na trening(80% pocetnog dataseta) i test(20% pocetnog dataseta)
set.seed(1)
train_indices <- createDataPartition(dataSub$Stayed, p = 0.8, list = FALSE)
train_data <- dataSub[train_indices,]
test_data <- dataSub[-train_indices,]

library(rpart)
library(e1071)

# definisemo da cemo za odredjivanje cp vrednosti koristiti 10-fold cross-validation
# definisemo u kom rasponu vredsnoti trazimo najbolju vrednost cp parametra
tr_ctrl <- trainControl(method = 'cv', number = 10)
cp_grid <- expand.grid(.cp = seq(0.01, 0.05, 0.001))

# pokrecemo kros-validaciju, a kako je u pitanju probabilisticki proces moramo podesiti seed vrednost 
set.seed(1)
cv <- train(x = train_data[, -7], y = train_data$Stayed, method = 'rpart', trControl = tr_ctrl, tuneGrid = cp_grid)
cv

# vidimo da nam je najbolja vrednost za cp parametar 0.023

best_cp <- cv$bestTune$cp

# kreiramo model sa nadjenom cp vrednoscu
set.seed(1)
model <- rpart(Stayed ~ ., data = train_data, control = rpart.control(cp = best_cp))
model
# pravimo predikcije
model_pred <- predict(model, newdata = test_data, type = 'class')

#kreiramo matricu konfuzije
cm <- table(actual = test_data$Stayed, predicted = model_pred)
cm
#iz evaluaciona matrice gledamo 4 vrednosti TP, TN, FP i FN:
# True-Positive(TP) - koliko klijenata smo predvideli da su ostali u banci i oni su zaista ostali, u nasem slucaju 749
# True-Negative(TN) - koliko klijenata smo predvideli da su napustili banku i oni su je zaista napustili, u nasem slucaju 91
# False-Positive(FP) - koliko klijenata smo predvideli da su ostali u banci a oni su je ipak napustili, u nasem slucaju 113
# False-Negative(FN) - koliko klijenata smo predvideli da su napustili banku, a oni su ipak ostali, u nasem slucaju 25


# definisemo funkciju za izracunavanje evaluacionih metrika
compute_eval_metrics <- function(cm){
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]
  a <- (TP + TN) / sum(cm)
  p <- TP / (TP + FP)
  r <- TP / (TP + FN)
  f1 <- 2*p*r/(p+r)
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}

# evaluacione metrike
eval <- compute_eval_metrics(cm)
eval
# racunamo 4 evaluacione matrike za nas model:
# 
# accuracy - koliko tacno predvidjamo izlaznu varijablu, u nasem slucaju od 978 klijenata mi smo tacno predvideli 807,
# odnosno, za 85.9% klijenata smo tacno predvideli da li ce ostati u banci ili ne
# 
# precision - od svih klijenata koje smo predvideli da ce ostati banci, koji procenta klijenata je zaista ostao,
# kako nam je FP 126, takodje vidimo da nam je ova metrika visoka i iznosi 0.87; odnosno, od svih klijenata za koje smo
# predvideli da ce ostati u banci, 87% njih je stvarno ostalo
#
# recall - od svih klijenata koji su zaista ostali u banci, koji procenat klijenata smo mi predvideli da ce ostati u banci
# kako nam je FN samo 45, mozemo videti nam je i ova metrika visoka odnosno 0.968; odnosno, medju svim klijentima koji su 
# zaista ostali u banci, mi smo tacno predvideli 96.8% njih
# 
# F1 - predstavlja meru performanski modela za balansirane vrednosti precision i recall, i za njom postoji potreba jer su
# precision i recall u antagonistickom odnosu, povecanje jedne, druga se smanjuje i obrnuto
# kako su i precision i recall visoki, i vrednost ove metrike je visokih 0.92. U kontekstu zadatka, ova metrika pokazuje 
# da smo predvidjanjima ne samo u najvecoj meri obuhvatili sve klijente koji su ostali u banci, vec i da nismo pravili
# mnogo gresaka predvidjajuci ostanak u banci onih koji su je zapravo napustili.     

## Napomena: neke od prethodnih formulacija su blago izmenjene, bez promene znacenja, kako bi se ucinile jasnijim i/ili 
## kompletnijim. VAZNO: date interpretacije tj nacin na koji su iskazane nije jedini korektan nacin, vec pokazuje samo 
## jedan od nacina da se formulisu interpretacije metrika kako bi se odgovorilo na zahtev zadatka. 
## 