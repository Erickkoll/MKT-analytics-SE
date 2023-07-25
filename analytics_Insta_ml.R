#Projeto de análise do Instagram do Startup Experience (SE) com agrupamento por clusterização

setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Startup-project/MKT-analytics-SE/MKT-analytics-SE")
getwd()

#pacotes

install.packages("lubridate")
install.packages("lattice")
install.packages("gridExtra")
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
        legend = FALSE)

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

# Mapa de Correlação
plot.cors(cors, "pearson")


max_iterations <- 500
iteration <- 1
condition_met <- FALSE

while(iteration <= max_iterations && !condition_met) {
  
  # Kmeans clusterização
  cluster <- kmeans(dados_cluster, centers = 3, iter.max = 100, nstart = 1)
  dados$Cluster <- cluster$cluster
  
  # Renomeando os clusters
  dados_agrup <- dados
  dados_agrup[, "Cluster"] <- ifelse(dados_agrup[, "Cluster"] == 1, "padrão",
                                     ifelse(dados_agrup[, "Cluster"] == 2, "good",
                                            ifelse(dados_agrup[, "Cluster"] == 3, "hype",
                                                   as.character(dados_agrup[, "Cluster"]))))
  
  # Verificando as condições
  min_good_alcance <- min(dados_agrup$Alcance[dados_agrup$Cluster == "good"])
  max_padrao_alcance <- max(dados_agrup$Alcance[dados_agrup$Cluster == "padrão"])
  
  min_hype_alcance <- min(dados_agrup$Alcance[dados_agrup$Cluster == "hype"])
  max_good_alcance <- max(dados_agrup$Alcance[dados_agrup$Cluster == "good"])
  
  # Obtendo o percentil 90 de alcance
  quantile_limit <- quantile(dados_agrup$Alcance, 0.9)
  
  if(min_good_alcance > max_padrao_alcance && 
     min_hype_alcance > max_good_alcance &&
     min_hype_alcance > quantile_limit) {
    condition_met <- TRUE
  } else {
    iteration <- iteration + 1
  }
  
}

if(!condition_met) {
  cat("Condição não satisfeita após", max_iterations, "iterações.\n")
} else {
  cat("Condição satisfeita após", iteration, "iterações.\n")
}

# Função para calcular as envoltórias convexas
compute_hull <- function(df) {
  hull_indices <- chull(df$Alcance, df$Curtidas)
  return(df[hull_indices, ])
}

# Definir a ordem dos níveis no fator "Cluster" em dados_agrup
dados_agrup$Cluster <- factor(dados_agrup$Cluster, levels = c("padrão", "good", "hype"))

# Definir pontos acima do 90º percentil de alcance como 'hype'
quantile_limit <- quantile(dados_agrup$Alcance, 0.95)
dados_agrup$Cluster[dados_agrup$Alcance > quantile_limit] <- "hype"

# Calcular as envoltórias convexas
hull_list <- lapply(split(dados_agrup, dados_agrup$Cluster), compute_hull)
hulls <- do.call(rbind, hull_list)

# Plotar os pontos e as delimitações
ggplot(dados_agrup, aes(x = Alcance, y = Curtidas)) +
  geom_point(aes(color = as.factor(Cluster))) +
  geom_polygon(data = hulls, aes(x = Alcance, y = Curtidas, fill = as.factor(Cluster)), alpha = 0.3) +
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 400), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 20), expand = c(0, 0)) +
  labs(title = "Alcance vs. Curtidas")

# 1. Verificar quantos pontos foram atribuídos ao cluster "hype"
cat("Número de pontos no cluster 'hype':", sum(dados_agrup$Cluster == "hype"), "\n")

# 2. Ajustar os limites do gráfico, se necessário.
# Vamos calcular os valores máximos de Alcance e Curtidas para determinar os limites do gráfico
max_alcance <- max(dados_agrup$Alcance)
max_curtidas <- max(dados_agrup$Curtidas)

# 3. Desenhar os pontos do cluster "hype"
# Ajustar a função compute_hull para retornar os pontos mesmo que não possam ser encapsulados por uma envoltória convexa
compute_hull_adjusted <- function(df) {
  if (nrow(df) >= 3) {
    hull_indices <- chull(df$Alcance, df$Curtidas)
    return(df[hull_indices, ])
  } else {
    return(df)  # Retorna os pontos mesmo que não possam ser encapsulados por uma envoltória convexa
  }
}

# Recalcular as envoltórias convexas
hull_list <- lapply(split(dados_agrup, dados_agrup$Cluster), compute_hull_adjusted)
hulls <- do.call(rbind, hull_list)

# Plotar os pontos e as delimitações com limites ajustados
ggplot(dados_agrup, aes(x = Alcance, y = Curtidas)) +
  geom_point(aes(color = as.factor(Cluster))) +
  geom_polygon(data = hulls, aes(x = Alcance, y = Curtidas, fill = as.factor(Cluster)), alpha = 0.3) +
  scale_x_continuous(limits = c(0, max_alcance), breaks = seq(0, max_alcance, by = 400), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, max_curtidas), breaks = seq(0, max_curtidas, by = 20), expand = c(0, 0)) +
  labs(title = "Alcance vs. Curtidas")

table(dados_agrup$Cluster)

# Visualizar a tabela com Dados e Rotulação dos Grupos
View(dados_agrup)