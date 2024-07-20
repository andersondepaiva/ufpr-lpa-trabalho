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
indices <- createDataPartition(data$tipo, p=0.80, list=FALSE) 
treino <- data[indices,]
teste <- data[-indices,]

### Gera um modelo usando Random Forest
set.seed(202437)
rf <- train(tipo~., data=treino, method="rf") 
rf

predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$tipo))

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

set.seed(202437)
rfCv <- train(tipo~., data=treino, method="rf", trControl=ctrl)
rfCv

predict.rfCv <- predict(rfCv, teste)
confusionMatrix(predict.rfCv, as.factor(teste$tipo))
