##libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

##Recuperar el archivo
booking <- read.csv("c:/Users/52333/Dropbox/Mi PC (LAPTOP-569HTKOE)/Documents/Prueba Git/TECP0013ADDAOL2112/Módulo 2/checkpoint/hotel_bookings.csv")
summary(booking)
dim(booking)
head(booking)
str(booking)
View(booking)


#Verificar NA
(colSums(is.na(booking)))

#Preparar dataframe
#cambiar a formato fecha reservation_status_date y convertir en factor is_canceled
bforest <- mutate(booking, reservation_status_date = as.Date(reservation_status_date, "%d/%m/%Y"),
                  is_canceled = factor(is_canceled))

#Cambiar fecha a formato date. Hotel, is_canceled, market segment a factor
b1 <- mutate(booking, reservation_status_date = as.Date(reservation_status_date, "%d/%m/%Y"),
             hotel = factor(hotel),
             is_canceled = factor(is_canceled),
             market_segment = factor(market_segment))

##DESCRIPCIÓN DE LOS DATOS
# Pernoctación por tipo de mercado
b1 %>%
  ggplot(aes(x= market_segment, y = stays_in_week_nights, fill=  hotel))+
  geom_boxplot()

#Cancelación por tipo de mercado
b1 %>%
  ggplot(aes(x=market_segment, y = stays_in_week_nights, fill = is_canceled))+
  geom_boxplot()

#cancelación por país
b1 %>%
  ggplot(aes(x=country, y = stays_in_week_nights, fill = is_canceled))+
  geom_boxplot()

# reservación por tipo de habitación
b1 %>%
  ggplot(aes(x=reserved_room_type, y = adr_normal, fill = hotel))+
  geom_boxplot()+
  facet_wrap("is_canceled") +
  ylim(0,250)

## VERIFICAR NORMALIDAD DE LOS DATOS
#frecuencia de tipo de segmento de mercado
freq_tab= table(b1$market_segment)
freq_tab
barplot(freq_tab)

plot(b1$is_canceled)

hist(b1$lead_time)

## PRUEBAS NO PARAMÉTRICAS
#Si existe correlación entre tipo de segmento y el que cancelen reservación
attach(booking)
t1 <- table(deposit_type, is_canceled)
plot(t1)
chisq.test(t1, simulate.p.value = TRUE)

## RANDOM FOREST
#Instalar paquetes
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source") 
library(randomForest)

#Crear semilla, datos test y datos tain
set.seed(101)
tamano.total <- nrow(bforest)
tamano.entreno <- round(tamano.total*0.7)
datos.indices <- sample(1:tamano.total , size=tamano.entreno)
datos.entreno <- bforest[datos.indices,]
datos.test <- bforest[-datos.indices,]

#Modelo Random Forest
modelo <- randomForest(is_canceled~., data=datos.entreno)
modelo

#gráficos y resultados
varImpPlot(modelo)
plot(modelo)
legend("right", colnames(modelo$err.rate), lty = 1:5, col = 1:6)
importance(modelo2)


## GRAFICAR LAS PRINCIPALES VARIABLES DE ACUERDO CON RANDOM FOREST
#agrupar por lead_time
grouplead_time <- aggregate(booking["is_canceled"], by=booking["lead_time"], mean)
grouplead_time

grouplead_time %>%
  ggplot(aes(x=lead_time, y = (is_canceled), color = is_canceled))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#agrupar por deposit_type
groupdeposit_type <- aggregate(booking["is_canceled"], by=booking["deposit_type"], mean)
groupdeposit_type

plot(factor(groupdeposit_type$deposit_type), groupdeposit_type$is_canceled)

groupdeposit_type %>%
  ggplot(aes(x= factor(deposit_type), y = is_canceled))+
  geom_bar(stat = "identity")


#agrupar por adr
groupadr <- aggregate(booking["is_canceled"], by=booking["adr"], mean)
groupadr

groupadr %>%
  ggplot(aes(x=adr, y = (is_canceled)))+
  geom_point()+
  xlim(0,500)+
  geom_smooth(method = "lm", se = FALSE)

#agrupar por market_segment
groupmarket_segment <- aggregate(booking["is_canceled"], by=booking["market_segment"], mean)
groupmarket_segment

plot(factor(groupmarket_segment$market_segment), groupmarket_segment$is_canceled)

groupmarket_segment %>%
  ggplot(aes(x= factor(market_segment), y = is_canceled))+
  geom_bar(stat = "identity")


#agrupar por total_specil_request
grouprequest <- aggregate(booking["is_canceled"], by=booking["total_of_special_requests"], mean)
grouprequest


grouprequest %>%
  ggplot(aes(x= factor(total_of_special_requests), y = is_canceled))+
  geom_point()



#### SVM
# Partición de los datos
set.seed(1)
df <- bforest
nobs <- nrow(bforest)
itrain <- sample(nobs, 0.8 * nobs)
train <- df[itrain, ]
test <- df[-itrain, ]

#Instalar paquete
install.packages("kernlab")
library(kernlab)
library(e1071)


## Preparar datos
set.seed(2022)
train = sample(nrow(bforest), 
               round(nrow(bforest)/2))
tail(bforest[train, ])

# Modelo con todo el data set
best <- svm(is_canceled~.,  data = bforest[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51)

best
summary(best)

#hacer plot


# svm con lead time y adr
best2 <- svm(is_canceled ~ lead_time + adr,  data = train,
             kernel = "radial",
             cost = 100,
             gamma = 1.51
)

best2
summary(best2)


#######Regresion logistica con todas las variables
#Regresión logística con todas las variables
rl <- glm(is_canceled ~., data = train)
rl
summary(rl)

#Regresión logística con las variables de random forest
rl2 <- glm(is_canceled ~ deposit_type + country + lead_time + market_segment + adr, family = binomial, data = train)
rl2
summary(rl2)

#Ajuste de la regresión logística sin la variable que no aporta
rl3 <- update(rl2, ~. -country)
summary(rl3)

#Ajuste de la regresión logística con la variable faltante
rl4 <- update(rl3, ~. +total_of_special_requests)
summary(rl4)
