# Carregando pacote que calcula a distribuicao de frequencia
# install.packages("DescTools")
library(DescTools)

# Carregas salarios.RData
load("salarios.RData")

# a. Média, Mediana e Moda das idades das esposas
mean_age <- mean(salarios$age)
median_age <- median(salarios$age)
mode_age <- Mode(salarios$age)

cat("Idades das esposas", "\n")
cat("Média:", mean_age, "\n")
cat("Mediana:", median_age, "\n")
cat("Moda of age:", mode_age, "\n")
cat("\n")

# a. Média, Mediana e Moda das idades dos maridos
mean_husage <- mean(salarios$husage)
median_husage <- median(salarios$husage)
mode_husage <- Mode(salarios$husage)

cat("Idades dos maridos", "\n")
cat("Média:", mean_husage, "\n")
cat("Mediana:", median_husage, "\n")
cat("Moda:", mode_husage, "\n")

# b. Variancia, desvio padrrao e coeficiente de variacao das idades das esposas
var_age <- var(salarios$age)
sd_age <- sd(salarios$age)
cv_age <- sd_age / mean_age

cat("Idades das esposas", "\n")
cat("Variância:", var_age, "\n")
cat("Desvio Padrão:", sd_age, "\n")
cat("Coeficiente de Variação:", cv_age, "\n")
cat("\n")

# b. Variancia, desvio padrrao e coeficiente de variacao das idades dos maridos
var_husage <- var(salarios$husage)
sd_husage <- sd(salarios$husage)
cv_husage <- sd_husage / mean_husage

cat("Idades dos maridos", "\n")
cat("Variância:", var_husage, "\n")
cat("Desvio Padrão:", sd_husage, "\n")
cat("Coeficiente de Variação:", cv_husage, "\n")