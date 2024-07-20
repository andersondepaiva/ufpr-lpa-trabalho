### Pacotes necessários:
install.packages("caret") 
install.packages("e1071") 
install.packages("mlbench") 
install.packages("mice")
install.packages("Metrics")
library("mlbench") 
library("caret") 
library("mice")
library("Metrics")

# Funcao para calcular R², Syx, and Syx%
calcular_metricas <- function(atual, predito) {

    # Residuais
    residuais <- atual - predito
    
    # Quadrado dos residuais
    ssr <- sum(residuais^2)

    # Total quadrados
    sst <- sum((atual - mean(atual))^2)

    # R²
    r_squared <- 1 - (ssr / sst)
    
    # Syx
    syx <- sqrt(ssr / (length(atual) - 2))

    # Pearson correlation coefficient
    pearson <- cor(atual, predito, method = "pearson")

    return(list(Rmse = rmse(atual, predito), Mae = mae(atual, predito), R2 = r_squared, Syx = syx, Pearson = pearson))
}


### Leitura dos dados
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/regressao/admissao/")
data <- read.csv("admissao.csv")

### Remove a coluna a
data$num <- NULL

### View(data)

### Cria arquivos de treino e teste
set.seed(202437)
indices <- createDataPartition(data$ChanceOfAdmit, p=0.80, list=FALSE) 
treino <- data[indices,]
teste <- data[-indices,]

### Gera modelo
set.seed(202437)
svm <- train(ChanceOfAdmit~., data=treino, method="svmRadial") 
svm

predict.svm <- predict(svm, teste)
metricas <- calcular_metricas(teste$ChanceOfAdmit, predict.svm)
metricas

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

set.seed(202437)
svmCv <- train(ChanceOfAdmit~., data=treino, method="svmRadial", trControl=ctrl)
svmCv

predict.svmCv <- predict(svmCv, teste)
metricasCv <- calcular_metricas(teste$ChanceOfAdmit, predict.svmCv)
metricasCv
