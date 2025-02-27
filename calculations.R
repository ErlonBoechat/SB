library(tidyverse)
library(R6)

# Classe para cálculos de métricas
MetricsCalculator <- R6::R6Class("MetricsCalculator",
                                 public = list(
                                   data = NULL,
                                   
                                   initialize = function(data) {
                                     if (is.null(data) || nrow(data) == 0) {
                                       stop("Os dados fornecidos estão vazios ou são inválidos.")
                                     }
                                     self$data <- data
                                   },
                                   
                                   calcular_medias = function() {
                                     if (is.null(self$data)) {
                                       stop("Nenhum dado disponível para calcular médias.")
                                     }
                                     
                                     self$data %>%
                                       group_by(Perfil) %>%
                                       summarise(
                                         `Media.Uploads` = mean(`Media.Uploads`, na.rm = TRUE),
                                         `Followers` = mean(`Followers`, na.rm = TRUE),
                                         `Following` = mean(`Following`, na.rm = TRUE),
                                         `Engagement.Rate` = mean(`Engagement.Rate`, na.rm = TRUE),
                                         `AVG.Likes` = mean(`AVG.Likes`, na.rm = TRUE),
                                         `AVG.Comments` = mean(`AVG.Comments`, na.rm = TRUE)
                                       )
                                   },
                                   
                                   calcular_variacoes = function() {
                                     if (is.null(self$data)) {
                                       stop("Nenhum dado disponível para calcular variações percentuais.")
                                     }
                                     
                                     self$data %>%
                                       arrange(Perfil, DataHora) %>%
                                       group_by(Perfil) %>%
                                       mutate(
                                         `Followers (%)` = (Followers - lag(Followers)) / lag(Followers) * 100,
                                         `Following (%)` = (Following - lag(Following)) / lag(Following) * 100,
                                         `AVG Likes (%)` = ( `AVG Likes` - lag(`AVG Likes`) ) / lag(`AVG Likes`) * 100,
                                         `AVG Comments (%)` = ( `AVG Comments` - lag(`AVG Comments`) ) / lag(`AVG Comments`) * 100,
                                         `Engagement Rate (%)` = ( `Engagement Rate` - lag(`Engagement Rate`) ) / lag(`Engagement Rate`) * 100,
                                         `Followers (30d) (%)` = ( `Followers (30d)` - lag(`Followers (30d)`) ) / lag(`Followers (30d)`) * 100,
                                         `Following (30d) (%)` = ( `Following (30d)` - lag(`Following (30d)`) ) / lag(`Following (30d)`) * 100,
                                         `Media (30d) (%)` = ( `Media (30d)` - lag(`Media (30d)`) ) / lag(`Media (30d)`) * 100
                                       )
                                   }
                                 )
)
