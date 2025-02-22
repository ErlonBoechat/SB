library(tidyverse)
library(readr)
library(lubridate)
library(R6)

# Classe para processar os dados da raspagem
DataProcessor <- R6::R6Class("DataProcessor",
                             public = list(
                               data = NULL,
                               
                               initialize = function(file_path) {
                                 if (!file.exists(file_path)) {
                                   stop("Arquivo não encontrado: ", file_path)
                                 }
                                 self$data <- self$read_data(file_path)
                               },
                               
                               read_data = function(file_path) {
                                 raw_lines <- read_lines(file_path)
                                 raw_lines <- trimws(raw_lines)  # Remove espaços extras
                                 
                                 # Remover caracteres invisíveis antes do @
                                 profile_raw_lines <- gsub("^[^@]*", "", raw_lines)
                                 
                                 # Filtrar apenas as linhas que começam com @
                                 profile_names <- profile_raw_lines[grepl("^@", profile_raw_lines)]
                                 rm(profile_raw_lines)

                                 # Contar a quantidade total de perfis
                                 num_profiles <- length(profile_names)
                                 
                                 if (num_profiles == 0) {
                                   stop("Erro: Nenhum perfil identificado no arquivo. Verifique o formato.")
                                  }
                                 
                                 print(paste("Perfis identificados:", num_profiles))
                                 
                                 # Lista das métricas esperadas
                                 metric_names <- c("Media Uploads", "Followers", "Following", "Engagement Rate", 
                                                   "AVG Likes", "AVG Comments")
                                 
                                 profile_data <- list() # Criar estrutura para armazenar dados do perfil
                                 profiles_data <- list()# Criar estrutura para armazenar os dados processados

                                 first_metric_position <- 5  # Linha onde começam as métricas do primeiro perfil
                                 r <- 47  # Razão fixa de espaçamento entre perfis
                                 
                                 for (i in 1:num_profiles) {  # Iterar sobre cada perfil identificado
                                   start_index <- first_metric_position + (i - 1) * r  # Posição inicial das métricas do perfil atual

                                   for (j in 1:length(metric_names)) { # Iterar sobre as métricas e armazenar os valores no perfil correspondente
                                      metric_index <- start_index + (j - 1) * 2  # Cada métrica está separada por 2 linhas
                                     
                                      if (metric_index <= length(raw_lines)) { # Verificar se a linha existe antes de tentar acessá-la
                                         value <- gsub("[^0-9.-]", "", raw_lines[metric_index])  # Extrair números
                                         profile_data[[metric_names[j]]] <- as.numeric(value)  # Converter para número
                                      } 
                                      
                                      else {
                                      
                                         profile_data[[metric_names[j]]] <- NA  # Evitar erro caso a linha não exista
                                      }
                                   }
                                   
                                     profiles_data[[Perfil = profile_names[i]]] <- profile_data # Armazena os dados do perfil na lista final
                                 }
                                 
                                 # Converter a lista para um dataframe final
                                 final_data <- do.call(rbind, lapply(profiles_data, as.data.frame))
                                 
                                 if (is.null(final_data) || nrow(final_data) == 0) {
                                   stop("Erro: Nenhum dado foi extraído do arquivo ", file_path)
                                 }
                                 
                                 return(final_data)
                               }
                             )
)
