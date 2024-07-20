### Pacotes necessários:
install.packages("caret") 
install.packages("e1071") 
install.packages("mlbench") 
install.packages("mice")
install.packages("Metrics")
install.packages("kernlab")
library("mlbench") 
library("caret") 
library("mice")
library("Metrics")
library("kernlab")

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

### Gera um modelo usando Random Forest
set.seed(202437)
rf <- train(ChanceOfAdmit~., data=treino, method="rf") 
rf

predict.rf <- predict(rf, teste)
metricas <- calcular_metricas(teste$ChanceOfAdmit, predict.rf)
metricas

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

set.seed(202437)
rfCv <- train(ChanceOfAdmit~., data=treino, method="rf", trControl=ctrl)
rfCv

predict.rfCv <- predict(rfCv, teste)
metricasCv <- calcular_metricas(teste$ChanceOfAdmit, predict.rfCv)
metricasCv
