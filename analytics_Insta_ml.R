#Projeto de análise do Instagram do Startup Experience (SE) com agrupamento por clusterização

setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Startup-project/MKT-analytics-SE")
getwd()

#pacotes

install.packages("lubridate")
install.packages("lattice")
library(lattice)
library(lubridate)
library(Amelia)
library(ggplot2)

#carregando dados

dados <- read.csv("Dados_postagem_MKT-Analytics-CPT.csv")
View(dados)

dados$Data <- dmy_hm(dados$Data)

#verificando valores missing

missmap(dados, 
        main = "Instagram do Startup Experience (SE) - Mapa de Dados Missing", 
        col = c("yellow", "black"), 
        legend = FALSE)boxplot(dados$Curtidas, main = "boxplot das curtidas por postagem", ylab = "curtidas")

#plot de análise temporal

ggplot(dados, aes(x = Data, y = Alcance)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Alcance pelo tempo",
       x = "Tempo",
       y = "Alcance")

#Boxplot

boxplot(dados$Curtidas, main = "boxplot das curtidas por postagem", ylab = "curtidas")
boxplot(dados$Comentários, main = "boxplot dos comentários por postagem", ylab = "comentários")
boxplot(dados$Alcance, main = "boxplot das Alcance por postagem", ylab = "Alcance")

#Resumo

summary(dados$Curtidas)
summary(dados$Comentários)
summary(dados$Alcance)

#Correlação

correlação_Curtidas_Alcance <- cor(dados$Curtidas,dados$Alcance, method = "pearson")
correlação_Curtidas_Alcance
correlação_Comentarios_Alcance <- cor(dados$Comentários,dados$Alcance, method = "pearson")
correlação_Comentarios_Alcance

cols <- c("Curtidas","Comentários","Alcance")
cors <- cor(dados[, cols], method = "pearson")

head(cors)

# Preprando o plot
require(lattice)
plot.cors <- function(x, labs){
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
plot.cors(cors, "pearson")



