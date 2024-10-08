#install.packages("plyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("repr")
#install.packages("glmnet")

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

load("trabalhosalarios.RData")

dat <- trabalhosalarios
dat$earns <- NULL
View(dat)

set.seed(712)

# Set indice para 80% de treinamento
index <- sample(1 : nrow(dat), 0.8 * nrow(dat))

# Base treinamento
train <- dat[index,]

# Base de Teste
test <- dat[-index,]

# Padronizacao das variaveis
cols <- c('husage', 'husearns', 'huseduc', 'hushrs',
         'age', 'educ', 'exper', 'lwage')

# Padronizar a base de treinamento e teste
pre_proc_val <- preProcess(train[,cols], 
                           method = c("center", "scale"))
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

# Vamos ver o sumario estatistico das variaveis 
# padronizadas de cada dataset
summary(train)
summary(test)

cols_reg = c('husage', 'husunion', 'husearns', 'huseduc', 
             'husblck', 'hushisp', 'hushrs', 'kidge6','age',
             'black', 'educ', 'hispanic', 'union',
             'exper', 'kidlt6', 'lwage')

dummies <- dummyVars(lwage ~ husage + husunion + husearns + huseduc + 
                    husblck + hushisp + hushrs + kidge6 + age + black + 
                    educ + hispanic + union + exper + kidlt6, 
                     data = dat[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies))
print(dim(test_dummies))


x = as.matrix(train_dummies)

y_train = train$lwage

x_test = as.matrix(test_dummies)

y_test = test$lwage

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# treinar o modelo
elastic_reg <- train(lwage ~ husage + husunion + husearns + huseduc + 
                    husblck + hushisp + hushrs + kidge6 + age + black + 
                    educ + hispanic + union + exper + kidlt6,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont)

# O melhor parametro alpha escolhido eh:
elastic_reg$bestTune

# E os parametros sao:
elastic_reg[["finalModel"]][["beta"]]

# Vamos calcular o R^2 dos valores verdadeiros e 
# preditos conforme a seguinte funcao:
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # As metricas de performace do modelo:
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# Vamos fazer as predicoes no modelo de treinamento:
predictions_train <- predict(elastic_reg, x)

# As metricas da base de treinamento sao:
eval_results(y_train, predictions_train, train)

# Predicao e avaliacao nos dados de teste:
predictions_test <- predict(elastic_reg, x_test)

# As metricas da base de teste sao:
eval_results(y_test, predictions_test, test)

# Vamos fazer uma predicao para:
# husage = 40 anos (idade do marido)
husage = (40-pre_proc_val[["mean"]][["husage"]])/
  pre_proc_val[["std"]][["husage"]]

husunion = 0

# husearns = 600 (rendimento do marido em US$)
husearns = (600-pre_proc_val[["mean"]][["husearns"]])/
  pre_proc_val[["std"]][["husearns"]]

# huseduc = 13 (anos de estudo do marido)
huseduc = (13-pre_proc_val[["mean"]][["huseduc"]])/
  pre_proc_val[["std"]][["huseduc"]]

# husblck = 1 (o marido eh preto)
husblck = 1

# hushisp = 0 (o marido nao eh hispanico)
hushisp = 0

# hushrs = 40 (o marido trabalha 40 horas semanais)
hushrs = (40-pre_proc_val[["mean"]][["hushrs"]])/
  pre_proc_val[["std"]][["hushrs"]]

# kidge6 = 1 (nao tem filhos maiores de 6 anos)
kidge6 = 1

# age = 38 anos (idade da esposa) 
age = (38-pre_proc_val[["mean"]][["age"]])/
  pre_proc_val[["std"]][["age"]]

# black = 0 (esposa nao eh preta)
black = 0

# educ = 13 (esposa possui 13 anos de estudo)
educ = (13-pre_proc_val[["mean"]][["educ"]])/
  pre_proc_val[["std"]][["educ"]]

# hispanic = 1 (esposa nao eh hispanica)
hispanic = 1

# union = 0 (o casal nao possui uniao registrada)
union = 0

# exper = 18 (esposa possui 18 anos de experiencia)
exper = (18-pre_proc_val[["mean"]][["exper"]])/
  pre_proc_val[["std"]][["exper"]]

# kidlt6 = 1 (possui filhos com menos de 6 anos)
kidlt6 = 1

# Vamos construir uma matriz de dados para a predicao
our_pred = as.matrix(data.frame(husage=husage, 
                                husunion=husunion,
                                husearns=husearns,
                                huseduc=huseduc,
                                husblck=husblck,
                                hushisp=hushisp,
                                hushrs=hushrs,
                                kidge6=kidge6,
                                age=age,
                                black=black,
                                educ=educ,
                                hispanic=hispanic,
                                union=union,
                                exper=exper,
                                kidlt6=kidlt6))

# Fazendo a predicao:
predict_our_elasticnet <- predict(elastic_reg,our_pred)

# O resultado da predicao eh:
predict_our_elasticnet


wage_pred_elasticnet=(predict_our_elasticnet*
                   pre_proc_val[["std"]][["lwage"]])+
  pre_proc_val[["mean"]][["lwage"]]

# O resultado eh:
wage_pred_elasticnet

# Aplicar antilog:
wage_pred_elasticnet_antilog <- exp(wage_pred_elasticnet)
wage_pred_elasticnet_antilog

# O intervalo de confianca para o nosso exemplo eh:
n <- nrow(train) 
m <- wage_pred_elasticnet_antilog
s <- pre_proc_val[["std"]][["lwage"]]
dam <- s/sqrt(n)
CIlwr_elasticnet <- m + (qnorm(0.025))*dam
CIupr_elasticnet <- m - (qnorm(0.025))*dam 

# Intervalos:
CIlwr_elasticnet
CIupr_elasticnet