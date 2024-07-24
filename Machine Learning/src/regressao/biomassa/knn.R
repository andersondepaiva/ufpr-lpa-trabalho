### Pacotes necessários:
install.packages("e1071") 
install.packages("caret")
install.packages("Metrics")
library("Metrics")
library("caret")

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
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/regressao/biomassa/")
data <- read.csv("biomassa.csv")

### View(data)

### Cria arquivos de treino e teste
set.seed(202437)
ran <- sample(1:nrow(data), 0.8 * nrow(data))
treino <- data[ran,]
teste <- data[-ran,]

### Cria um grid com vários valores para K e faz o 
### treinamento
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
set.seed(202437)
knn <- train(biomassa~., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e imprime metricas
predict.knn <- predict(knn, teste)

metricas <- calcular_metricas(teste$biomassa, predict.knn)
metricas

# Grafico de resíduos
residuals <- teste$biomassa - predict.knn
plot(predict.knn, residuals, xlab = "Valores Preditos", ylab = "Residuais", main = "Plot Residual")

#####################################
#########  Predicao Novos Casos #####
#####################################

### Leitura dos dados
dataNovo <- read.csv("biomassa-novos.csv")
View(dataNovo)

predict.knn <- predict(knn, dataNovo)

dataNovo$biomassa <- NULL

resultado <- cbind(dataNovo, predict.knn)
View(resultado)
