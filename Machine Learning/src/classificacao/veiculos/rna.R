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

### Gera modelo
set.seed(202437)
rna <- train(tipo~., data=treino, method="nnet",trace=FALSE)
rna

predicoes.rna <- predict(rna, teste)
confusionMatrix(predicoes.rna, as.factor(teste$tipo))

######################################
#########  Cross Validation  #########
######################################

ctrl <- trainControl(method = "cv", number = 10)

### executa RNA com cross validation
set.seed(202437)
rnaCv <- train(tipo~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)

rnaCv

predict.rnaCv <- predict(rnaCv, teste) 
confusionMatrix(predict.rnaCv, as.factor(teste$tipo))

### size, decay
grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(202437)

rnaCv <- train(
 form = tipo~. , 
 data = treino , 
 method = "nnet" , 
 tuneGrid = grid , 
 trControl = ctrl , 
 maxit = 2000,trace=FALSE) 

rnaCv
