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
library(cluster)
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

dados_cluster <- dados[,c(3,5)]
View(dados_cluster)

cluster <- kmeans(dados_cluster, centers = 2, iter.max = 100, nstart = 1,
                      algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                    "MacQueen"), trace=FALSE)

# Adicionar a coluna de clusters ao conjunto de dados original
dados$Cluster <- cluster$cluster

# Criar uma tabela de contagem de observações em cada cluster
tabela_clusters <- table(dados$Cluster)
V_C <- data.frame(dados$Cluster)
print(tabela_clusters)

dados_cluster$Alcance_ajustado <- (dados_cluster$Alcance + 2000)
dados_cluster$Curtidas_ajustadas <- (dados_cluster$Curtidas + 200)


# Plot dos agrupamentos
clusplot(dados_cluster, cluster$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0,
         main = "Gráfico de Agrupamentos",
         xlab = "Alcance",
         ylab = "Curtidas")


axis(side = 1, at = seq(0, 2000, by = 500))
axis(side = 2, at = seq(0, 200, by = 50))



