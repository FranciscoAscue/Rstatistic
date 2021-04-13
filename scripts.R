print(data)
head(data)
#Normalizar
var_dep <- data$Class
var_ind <- data
var_ind$Class <- NULL
head(var_ind)

var_ind_norm <- scale(var_ind)
data_norm <- data.frame(var_ind_norm, var_dep)

#Separar data
set.seed(20) 
index <- rownames(data_norm) #extraigo index
index_random <- sample(index) #reordeno index aleatoriamente
data_norm[1,1:5] #mostrar data random norm, [filas,columnas]
data_norm_random <- data_norm[index_random,]
dim(data_norm_random)#mostrar filas y columnas
data_train <- data_norm_random[1:228,]
data_testeo <- data_norm_random[229:303,]
dim(data_train)
dim(data_testeo)

#Crear el algoritmo y entrenarlo
library(neuralnet)
nn <- neuralnet(var_dep ~ sex + fbs - trestbps, data=data_train, hidden = c(4,4), linear.output = FALSE, threshold = 0.01)
nn
plot(nn)

# Testear su precisionn usando la data de testeo
dim(data_testeo)
ypred <- predict(nn, data_testeo[,1:13])
dim(ypred)
ypred_round <- round(ypred,digits = 0)
yobs <- data_testeo[,14]
ypred_round
table(ypred_round,yobs)
efficiencia <- (26+18)/(26+19+12+18)*100
print(efficiencia)
