library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(R6)

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
                                 raw_lines <- trimws(raw_lines) # Remover espaços extras
                                 
                                 for (line in raw_lines) {
                                   
                                   # Ler e armazenar DataHora
                                   if (grepl("^[0-9]{2}:[0-9]{2} [0-9]{2}/[0-9]{2}/[0-9]{4}$", line)) {
                                     
                                         current_datetime <- line 
                                   }
                                 }
                                
                                 # Remover caracteres invisíveis antes do @
                                 profile_raw_lines <- gsub("^[^@]*", "", raw_lines)
                                 # Filtrar apenas as linhas que começam com @
                                 profile_names <- profile_raw_lines[grepl("^@", profile_raw_lines)]
                                 rm(profile_raw_lines)

                                 num_profiles <- length(profile_names)
                                 
                                 if (num_profiles == 0) {
                                      stop("Erro: Nenhum perfil identificado no arquivo. Verifique o formato.")
                                 }
                                 
                                 print(paste("Perfis identificados:", num_profiles))
                                 
                                 current_datetime <- data.frame(Date.Time = rep(current_datetime, num_profiles))
                                 current_source <- data.frame(Media.Source = rep("IG", num_profiles))
                                 
                                 metric_names <- c("Media Uploads", "Followers", "Following", "Engagement Rate", 
                                                   "AVG Likes", "AVG Comments")
                                 
                                 profile_data <- list() 
                                 profiles_data <- list() 

                                 first_metric_position <- 5  
                                 r <- 47  
                                 
                                 for (i in 1:num_profiles) { # Iterar sobre cada perfil identificado e armazenar dados em profiles_data
                                   start_index <- first_metric_position + (i - 1) * r  

                                   for (j in 1:length(metric_names)) { # Iterar sobre as métricas e armazenar os valores em profile_data
                                      metric_index <- start_index + (j - 1) * 2  
                                     
                                      if (metric_index <= length(raw_lines)) { 
                                         value <- gsub("[^0-9.-]", "", raw_lines[metric_index])  
                                         
                                         profile_data[[metric_names[j]]] <- as.numeric(value)  
                                      } 
                                      else {
                                         profile_data[[metric_names[j]]] <- NA 
                                      }
                                   }
                                     profiles_data[[Perfil = profile_names[i]]] <- profile_data 
                                 }
                                 
                                 final_data <- profiles_data %>%   # Converter a lista profiles_data para um dataframe final
                                   lapply(as.data.frame) %>%
                                    do.call(rbind, .) %>%
                                    cbind(Media.Source = current_source, .) %>%
                                    cbind(Date.Time = current_datetime, .)
                                 
                                 if (is.null(final_data) || nrow(final_data) == 0) {
                                   stop("Erro: Nenhum dado foi extraído do arquivo ", file_path)
                                 }
                                 
                                 return(final_data)
                               }
                             )
)
