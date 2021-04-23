library(e1071)
library(caret)
library(corrplot)

houses = read.csv('train.csv')
houses[is.na(houses)]<-0

#Se obtienen solo las variables cuantitativas
CuantHouses <- houses[,c(4,5,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]

#Se modifica el dataset para que solo contenga variables que tengan datos en pies cuadrados, esto hace que nuestra data este normalizada y escalada
CuantHouses$GarageCars<-NULL
CuantHouses$YearRemodAdd<-NULL
CuantHouses$TotRmsAbvGrd<-NULL
CuantHouses$FullBath<-NULL
CuantHouses$GarageYrBlt<-NULL
CuantHouses$BedroomAbvGr<-NULL
CuantHouses$LotFrontage<-NULL
CuantHouses$YearBuilt<-NULL
CuantHouses$BsmtFullBath<-NULL
CuantHouses$HalfBath<-NULL
CuantHouses$BsmtHalfBath<-NULL
CuantHouses$KitchenAbvGr<-NULL
CuantHouses$Fireplaces<-NULL
CuantHouses$MiscVal<-NULL
CuantHouses$YrSold<-NULL
CuantHouses$MoSold<-NULL

M<-cor(CuantHouses)
corrplot.mixed(M, upper = "square")

#De la variables elegidas se quitan aquellas que tienen multicorrelacion
CuantHouses$TotalBsmtSF<-NULL
CuantHouses$X2ndFlrSF<-NULL
CuantHouses$BsmtUnfSF<-NULL

M<-cor(CuantHouses)
corrplot.mixed(M, upper = "square")

houses<- CuantHouses

houses$clasification <- ifelse(houses$SalePrice > 290000, 1, ifelse(houses$SalePrice>170000, 2, 3))
houses$SalePrice <- NULL

houses[2] <- lapply(houses[2], as.integer)

porciento <- 70/100
set.seed(1234)

economicas<-houses[houses$clasification==3,]
intermedias<-houses[houses$clasification==2,]
caras<-houses[houses$clasification==1,]


numFilasTrainEcon<-sample(nrow(economicas), porciento*nrow(economicas))
trainEcon<-economicas[numFilasTrainEcon,]

numFilasTrainInter<-sample(nrow(intermedias), porciento*nrow(intermedias))
trainInter<-intermedias[numFilasTrainInter,]

numFilasTrainCaras<-sample(nrow(caras), porciento*nrow(caras))
trainCaras<-caras[numFilasTrainCaras,]


training<-rbind(trainInter, trainEcon, trainCaras)
test<-houses[setdiff(rownames(houses),rownames(training)),]

table(training$clasification)
table(test$clasification)

