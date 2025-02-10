library(tidyverse)
library(ggplot2)
library(R6)

# Classe para visualização de dados
DataVisualizer <- R6::R6Class("DataVisualizer",
                              public = list(
                                data = NULL,
                                
                                initialize = function(data) {
                                  if (is.null(data) || nrow(data) == 0) {
                                    stop("Os dados fornecidos estão vazios ou são inválidos.")
                                  }
                                  self$data <- data
                                },
                                
                                plot_followers_growth = function() {
                                  ggplot(self$data, aes(x = DataHora, y = Followers, color = Perfil, group = Perfil)) +
                                    geom_line() +
                                    geom_point() +
                                    labs(title = "Crescimento de Seguidores ao Longo do Tempo",
                                         x = "Data",
                                         y = "Número de Seguidores") +
                                    theme_minimal()
                                },
                                
                                plot_engagement_rate = function() {
                                  ggplot(self$data, aes(x = Perfil, y = `Engagement Rate`, fill = Perfil)) +
                                    geom_bar(stat = "identity") +
                                    labs(title = "Taxa de Engajamento por Perfil",
                                         x = "Perfil",
                                         y = "Taxa de Engajamento (%)") +
                                    theme_minimal() +
                                    coord_flip()
                                },
                                
                                plot_avg_likes_vs_comments = function() {
                                  ggplot(self$data, aes(x = `AVG Likes`, y = `AVG Comments`, color = Perfil)) +
                                    geom_point(size = 3, alpha = 0.7) +
                                    labs(title = "Relação entre Média de Likes e Comentários",
                                         x = "Média de Likes",
                                         y = "Média de Comentários") +
                                    theme_minimal()
                                }
                              )
)
