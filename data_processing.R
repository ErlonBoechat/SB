library(tidyverse)
library(readr)
library(lubridate)

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
                                 
                                 # Filtrar linhas para remover caracteres irrelevantes antes do primeiro perfil
                                 # Remover caracteres invisíveis antes do @
                                 
                                 profile_raw_lines <- gsub("^[^@]*", "", raw_lines)
                                 
                                 # Filtrar apenas as linhas que começam com @
                                 profile_names <- profile_raw_lines[grepl("^@", profile_raw_lines)]
                                 
                                 # Contar a quantidade total de perfis
                                 num_profiles <- length(profile_names)
                                 
                                 # Identificar o primeiro perfil corretamente
                                 start_index <- which(grepl("^@", profile_raw_lines))[1]
                                 
                                 if (is.na(start_index)) {
                                   stop("Erro: Nenhum perfil identificado após limpeza. Verifique o formato.")
                                 }
                                 
                                 # Manter apenas os dados após o primeiro perfil
                                 raw_lines <- raw_lines[start_index:length(raw_lines)]

                                 
                                 # Variáveis temporárias para armazenar os perfis
                                 profiles <- list()
                                 current_profile <- NULL
                                 current_data <- list()
                                 current_datetime <- NULL
                                 metric_key <- NULL
                                 
                                 print("Lendo o arquivo...")
                                 print(head(raw_lines, 1000))  # Exibe as 20 primeiras linhas
                                 
                                 for (line in raw_lines) {  
                                   line <- trimws(line)
                                   
                                   # Detecta a Data e Hora da coleta
                                   if (grepl("^[0-9]{2}:[0-9]{2} [0-9]{2}/[0-9]{2}/[0-9]{4}$", line)) {
                                     if (!is.null(current_profile)) {
                                       profiles[[current_profile]] <- rbind(profiles[[current_profile]], as.data.frame(current_data, stringsAsFactors = FALSE))
                                     }
                                     current_datetime <- line
                                     current_profile <- NULL
                                     current_data <- list()
                                   }
                                   
                                   # Detecta o nome do perfil
                                   else if (nzchar(line) && grepl("^@", line)) {
                                     
                                     if (!is.null(current_profile)) {
                                       profiles[[current_profile]] <- rbind(profiles[[current_profile]], as.data.frame(current_data, stringsAsFactors = FALSE))
                                     }
                                     current_profile <- line
                                     current_data <- list(DataHora = current_datetime, Perfil = current_profile)
                                   }
                                   
                                   # Mapeia as métricas
                                   else {
                                     metric_names <- c("Media Uploads", "Followers", "Following", "Engagement Rate", "AVG Likes", "AVG Comments", 
                                                       "Followers Rank", "Following Rank", "Engagement Rank", "Media Rank", 
                                                       "Followers for the last 30 days", "Following for the last 30 days", "Media for the last 30 days")
                                     
                                     if (line %in% metric_names) {
                                       metric_key <- line
                                     } 
                                     else {
                                       value <- gsub("[^0-9.-]", "", line)
                                       value <- as.numeric(value)
                                       
                                       if (!is.na(value) && !is.null(metric_key)) {
                                         current_data[[metric_key]] <- value
                                       }
                                     }
                                   }
                                 }
                                 
                                 if (!is.null(current_profile)) {
                                   profiles[[current_profile]] <- rbind(profiles[[current_profile]], as.data.frame(current_data, stringsAsFactors = FALSE))
                                 }
                                 
                                 final_data <- do.call(rbind, profiles)
                                 
                                 if (is.null(final_data) || nrow(final_data) == 0) {
                                   stop("Erro: Nenhum dado foi extraído do arquivo ", file_path)
                                 }
                                 
                                 print("Dados processados antes de transformar em dataframe:")
                                 print(profiles)
                                 return(final_data)
                               } 

  )
)
