#vjezba - neuralne mreže
produkt = read.table("/home/nola/Desktop/Files/Product.txt", header = TRUE, sep = "\t")
korisnici = read.csv("/home/nola/Desktop/Files/Customer.csv", header = TRUE)
View(korisnici)

y = table(korisnici$Region)
y
View(y)
barplot(y)
barplot(y[order(y)]) #uređen plot
barplot(y[order(y)], horiz = TRUE) #horizontalni barplot
#različite boje
barplot(y[order(y)], horiz = TRUE, col=c("orange", "blue", "red", "green"))
#micanje okvira stupaca
barplot(y[order(y)], horiz = TRUE, col=c("orange", "blue", "red", "green"), 
        border=NA, main = "Barplot \n frekvencija regija",
        xlab = "Frekvencije", ylab = "Regije")
#histogram
hist(korisnici$Age)
#grupirati stupce
hist(korisnici$Age, breaks = 5)
#dodati dužine kategorija
hist(korisnici$Age, breaks = c(0,40,60,100), freq=TRUE, col=c("yellow", "orange", "red"),
     main="Histogram frekvencije prema godinama", xlab = "Godine", ylab = "Frekvencija")
library(keras)
fashion = dataset_fashion_mnist()
#pridruzivanje x i y varijablama
c(train_slike, train_oznake) %<-% fashion$train 
c(test_slike, test_oznake) %<-% fashion$test
dim(train_slike)
str(train_slike)
#vizualizacija slike
objekt = train_slike[5,,]#peta slika
plot(as.raster(objekt, max=255)) #pikselizirana slika

#klase
imena = c("majica/top", "gaće", "pulover", "haljina", "kaput", "sandale", "majica", "patike", "torba", "čizme")
imena[train_oznake[5]+1]#ide +1 jer oznake počinju s 0
#normalizacija podataka - (x-mean)/std - ova formula se koristi kada psotoje heterogeni pdoaci
train_slike = train_slike/255
test_slike = test_slike/255 #dilimo sa 255 kako bi sve vrijednosti bile između 0 i 1

#izdvajanje podataka za validaciju
indeks = 1:5000
val_slike = train_slike[indeks,,]
part_train_slike = train_slike[-indeks,,]
val_oznake = train_oznake[indeks]
part_train_oznake = train_oznake[-indeks]
#sekvencijalni model se radi prilikon izrade kompleksnih mreža (koje se sastoje od više istih), sekvencijalni za običnu 
model = keras_model_sequential()

#struktura modela
model %>%
  layer_flatten(input_shape = c(28,28)) %>% #za pretvaranje u jednu dimenziju
  layer_dense(units = 128, activation = "relu") %>% #svaka linija za jedan sloj, ode je 1
  layer_dense(units = 10, activation = "softmax") #output sloj, 10 zbog 10 klasa

#konfiguracija procesa učenja
model %>% compile(optimizer = "sgd", 
                  loss = "sparse_categorical_crossentropy", 
                  metrics = c("accuracy")) # zadnji nije obavezan ali se koristi za pregled performanse modela
#binary crossentropy - ako objekt pripada jednoj od ukupno dvi klase
#sparse categorical crossentropy - koristi se kada imamo više od 2 klase i objekt pripada isključivo jednoj od tih
#categorical crossentropy - kad imamo više klasa i objekt može pripasti više od jedne

#kreiranje modela
model %>% fit(part_train_slike, part_train_oznake, epochs = 30, batch_size = 100, #broj opservacija kod svake forawrd i backward propagacije,
              validation_data = list(val_slike, val_oznake))

#evaluacija modela
score = model %>% evaluate(test_slike, test_oznake)
#cat("test loss:", score$loss, "\n")

#predikcija
predikcija = model %>% predict(test_slike)
predikcija[1,]
which.max(predikcija[1,])#najveća vjerojatnost klase
imena[which.max(predikcija[1,])]
plot(as.raster(test_slike[1,,], max=1))

#predikcija klase
predikcija_klase = model %>% predict_classes(test_slike)
predikcija_klase[1:20]
predikcija_klase[1:20] == test_oznake[1:20]

#flattening - xxx
#             yyy --> xxxyyyzzz
#             zzz

#neuralnet paket
library(neuralnet)
sati = c(20,10,30, 20, 50, 30)
test = c(90, 20, 20, 10, 50, 80)
prolaz = c(1, 0, 0, 0, 1, 1)
df = data.frame(sati, test, prolaz)
nn = neuralnet(prolaz~sati+test, data = df, hidden = c(3,2), 
               act.fct = "logistic", linear.output = FALSE) #false za klasifikaciju
plot(nn)
#crne su te�ine, plave biasi
tsati = c(20, 20, 30)
ttest = c(80, 30, 20)
testdf = data.frame(tsati, ttest)

predikcija = compute(nn, testdf)
prob = predikcija$net.result
pred = ifelse(prob > 0.5, 1, 0)
pred

#funkcional api
library(keras)
boston = dataset_boston_housing()
c(train_data, train_oznake) %<-% boston$train
c(test_data, test_oznake) %<-% boston$test

#normalizacija podataka
train_data = scale(train_data) #koristi onu formulu
#za normalizaciju test podataka koristimo mean i std train podataka
mean_train = attr(train_data, "scaled:center")
std_train = attr(train_data, "scaled:scale")
test_data = scale(test_data, center = mean_train, scale = std_train)

#definiranje inputa
input = layer_input(shape = dim(train_data)[2])#2 zato šta to predstavlja broj varijabli
#output sloj
predikcije = input %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) #nema aktivacije jer je regresija u pitanju

model = keras_model(inputs = input, outputs = predikcije)
model %>% compile(optimizer = "rmsprop", loss = "mse", metrics = "mean_absolute_error")
model %>% fit(train_data, train_oznake, epochs = 30, batch_size = 100)

#testiranje modela
score = model %>% evaluate(test_data, test_oznake)
score$mean_absolute_error

#kompleksne arhitekture - funkcionalni api
input_f = layer_input(shape = dim(train_data)[2])
predikcije_f = input_f %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") 

#dodatni sloj
main_output = layer_concatenate(c(predikcije_f, input_f)) %>%
  layer_dense(units = 1)

#kreiranje modela
model_f = keras_model(inputs = input_f, outputs = main_output) 
model_f %>% compile(optimizer = "rmsprop", loss="mse", metrics = list("mean_absolute_error"))
summary(model_f)
model_f %>% fit(train_data, train_oznake, epochs = 30, batch_size = 100)
score_f = model_f %>% evaluate(test_data, test_oznake)
save_model_hdf5(model_f, "model.h5")

#za spremanje istog u drugu varijablu koristimo funkciju load_model_hdf5

#callbackovi na rezutlate pojedine epohe
checkpoint_dir = "checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE)
filepath = file.path(checkpoint_dir, "Epoch-{epoch:02d}.hdf5")#svaka epoha zvat će se epoch - 01,02,...
cp_callback = callback_model_checkpoint(filepath = filepath)
rm(model_f)
k_clear_session() #cisti memoriju, počinje bez označenih težina
model_cb = keras_model(inputs = input_f, outputs = main_output)
model_cb %>% compile(optimizer = "rmsprop", loss="mse", metrics = list("mean_absolute_error"))
model_cb %>% fit(train_data, train_oznake, epochs = 30, callbacks = list(cp_callback))
  
deseti_model = load_model_hdf5(file.path(checkpoint_dir, "Epoch-10.hdf5"))
summary(deseti_model)

#očuvanje najboljeg modela
cp_callback_naj = callback_model_checkpoint(filepath = "najbolji.h5", monitor = "val_loss",
                                        save_best_only = TRUE)#kod klasifikacije se nadgleda accuracy
rm(model_cb)
k_clear_session()
model_cb_naj = keras_model(inputs = input_f, outputs = main_output)
model_cb_naj %>% compile(optimizer = "rmsprop", loss="mse", metrics = list("mean_absolute_error"))
model_cb_naj %>% fit(train_data, train_oznake, epochs = 30, validation_data = list(test_data, test_oznake), 
                     callbacks = list(cp_callback_naj))
naj_model = load_model_hdf5("najbolji.h5")

#stopiranje kad dođemo do najboljeg modela
callacks_list = list(callback_early_stopping(monitor = "val_loss", patience = 3), #koliko epoha gledamo jel ima napretka
                     callback_model_checkpoint(filepath = "najbolji_stopirani.h5", monitor = "val_loss", save_best_only = TRUE))
rm(model_cb_naj)
k_clear_session()
model_cb_stop = keras_model(inputs = input_f, outputs = main_output)
model_cb_stop %>% compile(optimizer = "rmsprop", loss="mse", metrics = list("mean_absolute_error"))
model_cb_stop %>% fit(train_data, train_oznake, epochs = 100, validation_data = list(test_data, test_oznake), 
                     callbacks = callacks_list)
naj = load_model_hdf5("najbolji_stopirani.h5")
k_clear_session()

##ANALIZA CIJENE KUĆA
df = read.csv("/home/nola/Documents/Files/Files for add-on section/Data/House_Price.csv", header = TRUE)
str(df)
summary(df) #univarijantna analiza podataka
#gledamo razliku između kvartala za svaku varijablu kako bi identificirali outliere i skewness
#kako bi saznali šta nije u redu tribamo plotat varijable
hist(df$crime_rate)
pairs(~df$price+df$crime+df$n_hot_rooms+df$rainfall)
#barplotovi kategoričkih varijabli
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

#outlieri - n_hot_rooms i rainfal, missing values - n_hos_beds, bus_ter je beskorisna, crime_rate ima skewness (druga funkcionalna veza)
#kad ima outliere - velika je razlika između meana i mediana
#outlieri
quantile(df$n_hot_rooms, 0.99)
uv = 3*15.39952
df$n_hot_rooms[df$n_hot_rooms>uv]=uv
summary(df$n_hot_rooms)

lv=0.3 * quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall<lv]=lv
summary(df$rainfall)

#missing values
mean(df$n_hos_beds, na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]=mean(df$n_hos_beds, na.rm = TRUE)
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))

#transofrmacija varijable crime_rate kako bi imala linearan odnos sa cijenom
attach(df)
pairs(~price + crime_rate)
plot(price, crime_rate)
crime_rate = log(1+crime_rate)
plot(price, crime_rate)

df$avg_dist = (dist1 + dist2 + dist3 + dist4)/4
df = df[,c(-7,-8,-9,-10)] #ili -7:-10
df = df[,-14]

#dummy varijable - na inkorporaciju kategoričkih varijabli u regresiju
#to je ono poseban stupac za svaku kategoriju
#ne postoji hijerarhija u ovin varijablama, ne mogu se kodirati brojevima jer se oni mogu poredati po veličini
#airport i waterbody convertiramo u dummy varijable
library(dummies)
df = dummy.data.frame(df)
df = df[,c(-9,-15)] #ove micemo jer su redundantne - npr kod airporta se vidi po nuli u stupcu yes da nema itd.

#korelacijska matrica
cor(df)
round(cor(df),2)
#ako nađemo neovisne vrijable koje su visoko kolerirane, tribamo jednu izbacit da izbjegnemo multicolinearnost
#izbacujemo onu koja ima veću korelaciju sa ovisnon varijablon
df = df[, -16]

#linearna regresija
s_model = lm(price~room_num)
summary(s_model)
plot(room_num, price)
abline(s_model)

#multiple linear regresija
m_model = lm(price ~ ., data = df)
summary(m_model)

#train split
library(caTools)
set.seed(0)
split_i = sample.split(df, 0.8)
train_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

lm_e = lm(price~., data = train_set, family="gaussian")
summary(lm_e)
train_e = predict(lm_e, train_set)
test_e = predict(lm_e, test_set)

mean((train_set$price-train_e)^2)
mean((test_set$price-test_e)^2)
