### Pacotes necessários:
install.packages("e1071") 
install.packages("caret")
install.packages("arules")
library("caret")
library("arules")

### Leitura dos dados
setwd("/Workspace/Afp/ufpr-lpa-trabalho/Machine Learning/src/associacao/")
data <- read.csv("musculacao.csv", sep = ";", header = FALSE)

### View(data)

set.seed(202437)
rules <- apriori(data, parameter = list(supp = 0.001, conf = 0.7, minlen = 2))
summary(rules)

### Visualizar regras
options(digits=2)
inspect(sort(rules, by="confidence"))
