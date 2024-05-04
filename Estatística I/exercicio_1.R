# Carregando pacote que calcula a distribuicao de frequencia
# install.packages("fdth")
# install.packages("car")
library(fdth)
library(car)

# Load the salarios.RData file
load("salarios.RData")

# a. Box-plot e histograma para a idade das esposas
hist(salarios$age, breaks = "Sturges", main = "Histograma por Idade da Esposa", xlab = "Idade Esposa")
Boxplot( ~ age, data=salarios, id=list(method="y"), 
         ylab="Idade esposas")

# a. Box-plot e histograma para a idade dos maridos
hist(salarios$husage, breaks = "Sturges", main = "Histograma por Idade do Marido", xlab = "Idade Marido")
Boxplot( ~ husage, data=salarios, id=list(method="y"), 
         ylab="Idade maridos")

# ---------------- || ---------------- || ---------------- || -------------------------------- #

# b. Tabela frequencia idades esposas
tableByAge <- fdt(salarios$age)
print(tableByAge)

# b. Tabela frequencia idades maridos
tableByHusage <- fdt(salarios$husage)
print(tableByHusage)