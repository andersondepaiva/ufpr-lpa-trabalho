### Pacotes necessários:
install.packages("mlbench")
library(mlbench)

install.packages("mice") 
library(mice)

## para o kmodes
install.packages("klaR")
library(klaR)

### Leitura dos dados
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/agrupamento/")
data <- read.csv("veiculos.csv")

### View(data)

### Agrupar por 10 clusters
set.seed(202437)
cluster.results <- kmodes(data, 10, iter.max = 10, weighted = FALSE ) 
cluster.results

resultado <- cbind(data, cluster.results$cluster)
resultado
