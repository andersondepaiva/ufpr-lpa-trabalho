### Pacotes necessários:
install.packages("e1071") 
install.packages("caret")
library("caret")

### Leitura dos dados
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/classificacao/veiculos/")
data <- read.csv("veiculos.csv")

### Remove a coluna a
data$a <- NULL

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
knn <- train(tipo ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Faz a predição e mostra a matriz de confusão
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$tipo))

