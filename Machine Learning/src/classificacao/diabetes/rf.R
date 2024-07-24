### Pacotes necessários:
install.packages("e1071") 
install.packages("caret")
library("caret")

### Leitura dos dados
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/classificacao/diabetes/")
data <- read.csv("diabetes.csv")

### Remove a coluna num
data$num <- NULL

### View(data)

### Cria arquivos de treino e teste
set.seed(202437)
indices <- createDataPartition(data$diabetes, p=0.80, list=FALSE) 
treino <- data[indices,]
teste <- data[-indices,]

### Gera um modelo usando Random Forest
set.seed(202437)
rf <- train(diabetes~., data=treino, method="rf") 
rf

predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$diabetes))

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

set.seed(202437)
rfCv <- train(diabetes~., data=treino, method="rf", trControl=ctrl)
rfCv

predict.rfCv <- predict(rfCv, teste)
confusionMatrix(predict.rfCv, as.factor(teste$diabetes))

####################################################
#########  Predicao Novos Casos - RF Hold-out  #####
####################################################

### Leitura dos dados
dataNovo <- read.csv("diabetes-novos.csv")
View(dataNovo)

### Remove a coluna a
dataNovo$num <- NULL

predict.rf <- predict(rf, dataNovo)

dataNovo$diabetes <- NULL

resultado <- cbind(dataNovo, predict.rf)
View(resultado)
