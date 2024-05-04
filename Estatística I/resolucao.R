# Carregando pacote que calcula a distribuicao de frequencia
# install.packages("DescTools")
library(DescTools)
library(fdth)
library(car)
library(rcompanion)
library("BSDA")
library("onewaytests")
library("sjPlot")
library("tigerstats")
library("misty")


# Carregar salarios.RData
load("salarios.RData")

# 1 graficos e tabelas

# a. Box-plot e histograma para a idade das esposas
hist(salarios$age)
plotNormalHistogram(salarios$age, prob = FALSE, 
                    main = "Histograma por Idade da Esposa", 
                    length = 1000 )

boxplot(salarios$age, main= "Idade das esposas")

# a. Box-plot e histograma para a idade dos maridos

hist(salarios$husage)
plotNormalHistogram(salarios$husage, prob = FALSE, 
                    main = "Histograma por Idade do Marido", 
                    length = 1000 )
                    
boxplot(salarios$husage, main= "Idade dos maridos")

# b. Tabela frequencia idades esposas
table_by_age <- fdt(salarios$age)
cat("Tabela Frequencia Esposas", "\n")
print(table_by_age)

# b. Tabela frequencia idades maridos
table_bt_husage <- fdt(salarios$husage)
cat("Tabela Frequencia Maridos", "\n")
print(table_bt_husage)

#########

# 2 Medias de posicao e dispersao

# a. Média, Mediana e Moda das idades das esposas
mean_age <- mean(salarios$age)
median_age <- median(salarios$age)
mode_age <- Mode(salarios$age)

cat("Idades das esposas", "\n")
cat("Média:", mean_age, "\n")
cat("Mediana:", median_age, "\n")
cat("Moda:", mode_age, "\n")
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

#########

# 3 Testes parametricos e não parametricos
set.seed(1985)

# Teste de normalidade Shapiro-Wilk
amostra <- sample_n(salarios, 5000)
with(salarios, shapiro.test(amostra$age))
with(salarios, shapiro.test(amostra$husage))

# Unpaired Test
#t_test_result <- t.test(salarios$age, salarios$husage, paired = FALSE)
#print(t_test_result)

# Mann-Whitney
wilcox_test_result <- wilcox.test(salarios$age, salarios$husage, exact=FALSE, conf.int=TRUE)
print(wilcox_test_result)