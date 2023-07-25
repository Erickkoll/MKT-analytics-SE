#Projeto de análise do Instagram do Startup Experience (SE) com agrupamento por clusterização

setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Startup-project/MKT-analytics-SE")
getwd()

install.packages("lubridate")
library(lubridate)
library(Amelia)
library(ggplot2)

dados <- read.csv("Dados_postagem_MKT-Analytics-CPT.csv")
View(dados)

dados$Data <- dmy_hm(dados$Data)

missmap(dados, 
        main = "Instagram do Startup Experience (SE) - Mapa de Dados Missing", 
        col = c("yellow", "black"), 
        legend = FALSE)boxplot(dados$Curtidas, main = "boxplot das curtidas por postagem", ylab = "curtidas")

ggplot(dados, aes(x = Data, y = Alcance)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Alcance pelo tempo",
       x = "Tempo",
       y = "Alcance")

boxplot(dados$Curtidas, main = "boxplot das curtidas por postagem", ylab = "curtidas")
boxplot(dados$Comentários, main = "boxplot dos comentários por postagem", ylab = "comentários")
boxplot(dados$Alcance, main = "boxplot das Alcance por postagem", ylab = "Alcance")

summary(dados$Curtidas)
summary(dados$Comentários)
summary(dados$Alcance)

