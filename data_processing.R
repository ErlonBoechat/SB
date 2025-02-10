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
                                 
                                 profiles <- list()
                                 current_profile <- NULL
                                 current_data <- list()
                                 current_datetime <- NULL
                                 
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
                                   else if (grepl("^@", line)) {
                                     if (!is.null(current_profile)) {
                                       profiles[[current_profile]] <- rbind(profiles[[current_profile]], as.data.frame(current_data, stringsAsFactors = FALSE))
                                     }
                                     current_profile <- line
                                     current_data <- list(DataHora = current_datetime, Perfil = current_profile)
                                   }
                                   
                                   # Mapeia as métricas
                                   else {
                                     metric_names <- c("Media Uploads", "Followers", "Following", "Engagement Rate", "AVG Likes", "AVG Comments", 
                                                       "Followers (30d)", "Following (30d)", "Media (30d)")
                                     
                                     if (line %in% metric_names) {
                                       metric_key <- line
                                     } else {
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
                                 return(final_data)
                               }
                             )
)
