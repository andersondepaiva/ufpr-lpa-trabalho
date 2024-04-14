library(mlbench)
library(caret)
library(neuralnet)
library(randomForest)
library(e1071)

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
    
    # Syx%
    syx_percentage <- (syx / mean(atual)) * 100

    return(list(R2 = r_squared, Syx = syx, Syx_percent = syx_percentage))
}

# Ler arquivo CSV
data <- read.csv2("Volumes.csv")

# Remover coluna NR
data$NR <- NULL

set.seed(7) 

# Criar particao 80 treinamento e 20 testes
train_indices <- createDataPartition(data$VOL, p=0.80, list=FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Treinar RandomForest
rf_model <- randomForest(VOL ~ ., data = train_data)
rf_predictions <- predict(rf_model, test_data)

# Treinar SVM
svm_model <- svm(VOL ~ ., data = train_data)
svm_predictions <- predict(svm_model, test_data)

# Treinar Neural Network
print("Treinando RNA...")
nn_model <- train(VOL ~ ., data=train_data, method="nnet", trace=FALSE)
nn_predictions <- predict(nn_model, test_data)

# Treinar SPURR
spurr_model <- nls(VOL ~ b0 + b1*DAP*DAP*HT, train_data, start=list(b0=0.5, b1=0.5))
spurr_predictions <- predict(spurr_model, test_data)

# Calcular e imprimir metricas RandomForest
rf_metrics <- calcular_metricas(test_data$VOL, rf_predictions)
print("Metrics for RandomForest model:")
print(rf_metrics)

# Calcular e imprimir metricas SVM
svm_metrics <- calcular_metricas(test_data$VOL, svm_predictions)
print("Metrics for SVM model:")
print(svm_metrics)

# Calcular e imprimir metricas Neural Network
nn_metrics <- calcular_metricas(test_data$VOL, nn_predictions)
print("Metrics for Neural Network model:")
print(nn_metrics)

# Calcular e imprimir metricas SPURR
spurr_metrics <- calcular_metricas(test_data$VOL, spurr_predictions)
print("Metrics for SPURR model:")
print(spurr_metrics)