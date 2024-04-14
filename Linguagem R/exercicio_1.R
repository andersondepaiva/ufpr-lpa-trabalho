library(mlbench)
library(caret)
library(neuralnet)
library(randomForest)
library(e1071)

data(Satellite)

set.seed(7)

# Criar particao 80 treinamento e 20 testes
trainIndex <- createDataPartition(Satellite$classes, p = 0.8, list = FALSE)
trainData <- Satellite[trainIndex, ]
testData <- Satellite[-trainIndex, ]

# Treinar RandomForest e imprimir matriz confusao
rf_model <- randomForest(classes ~ ., data = trainData)
rf_pred <- predict(rf_model, testData)
cf_rf <- confusionMatrix(rf_pred, testData$classes)
print("Matriz Confusao RF")
print(cf_rf)

# Treinar SVM e imprimir matriz confusao
svm_model <- svm(classes ~ ., data = trainData)
svm_pred <- predict(svm_model, testData)
svm_cm <- confusionMatrix(svm_pred, testData$classes)
print("Matriz Confusao SVM")
print(svm_cm)

# Treinar RNA e imprimir matriz confusao
print("Treinando RNA...")
rna_model <- train(classes ~ ., data=trainData, method="nnet", trace=FALSE)
rna_pred <- predict(rna_model, testData)
rna_cm <- confusionMatrix(rna_pred, testData$classes)
print("Matriz Confusao RNA")
print(rna_cm)

