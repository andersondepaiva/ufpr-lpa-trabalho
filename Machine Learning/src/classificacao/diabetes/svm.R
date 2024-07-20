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

### Gera modelo
set.seed(202437)
svm <- train(diabetes~., data=treino, method="svmRadial") 
svm

predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$diabetes))

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

set.seed(202437)
svmCv <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl)
svmCv

predict.svmCv <- predict(svmCv, teste)
confusionMatrix(predict.svmCv, as.factor(teste$diabetes))
