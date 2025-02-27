library(dplyr)
library(tidyverse)
library(R6)
library(lubridate)
source("data_processing.R")
source("calculations.R")
source("visualizations.R")
source("export.R")

# Defina o diretório onde os arquivos estão armazenados
dir_path <- "C:/Users/Erlon/OneDrive/dataLab/CORRIDA_GOV_2026/"

# Especifique o intervalo de arquivos desejado
start_file <- 25
end_file   <- 31

# Gere os nomes dos arquivos com base no padrão padronizado
file_names <- paste0("raspagem", start_file:end_file, ".txt")
file_paths <- file.path(dir_path, file_names)

# Processa cada arquivo e combina os dados
all_data <- do.call(rbind, lapply(file_paths, function(f) {
  message("Processando: ", f)
  processor <- DataProcessor$new(f)
  processor$data
}))

# Exemplo de processamento adicional: cálculo de uma nova coluna (soma de AVG Likes e AVG Comments)
all_data <- all_data %>%
  mutate(SUM_AVG_Engajament = rowSums(select(., `AVG.Likes`, `AVG.Comments`), na.rm = TRUE)) %>%
  arrange(desc(SUM_AVG_Engajament))


# Para cada arquivo, processa e adiciona as colunas 'Perfil' e 'Arquivo'
data_list <- lapply(file_paths, function(f) {
  message("Processando: ", f)
  processor <- DataProcessor$new(f)
  data <- processor$data
  
  # Converte os nomes das linhas (que contêm o perfil) em uma coluna
  data <- data %>% 
    mutate(Perfil = rownames(data),
           Arquivo = basename(f))
  
  # Remove os rownames, pois agora temos a coluna 'Perfil'
  rownames(data) <- NULL
  return(data)
})

# Junta os dados de todos os arquivos (bind_rows descarta rownames e mantém as colunas)
final_data <- dplyr::bind_rows(data_list)
# Desloca a coluna Perfil para a esquerda do dataframe
final_data <- final_data %>% select(Perfil, everything())

gera_tabela_medias <- function(final_data) {
  # Converter a coluna Date.Time para objeto de data/hora

  final_data <- final_data %>%
    mutate(Date.Time = parse_date_time(Date.Time, orders = "HM dmy", tz = "America/Sao_Paulo"))
  
  # Agregar os dados por 'Perfil'
  tabela <- final_data %>%
    group_by(Perfil) %>%
    reframe(
      # Para estas colunas, pegar o valor da última atualização (linha com maior Date.Time)
      `Media.Uploads` = `Media.Uploads`[which.max(Date.Time)],
      Followers = Followers[which.max(Date.Time)],
      Following = Following[which.max(Date.Time)],
      
      # Para as demais métricas, calcular a média dos valores
      `Engagement.Rate` = mean(`Engagement.Rate`, na.rm = TRUE),
      `AVG.Likes` = mean(`AVG.Likes`, na.rm = TRUE),
      `AVG.Comments` = mean(`AVG.Comments`, na.rm = TRUE),
      .groups = "drop"
    )
  return(tabela)
}

# Exemplo de uso:
tabela_medias <- gera_tabela_medias(final_data)
print(tabela_medias)


tabela_medias <- tabela_medias %>%
  arrange(desc(Engagement.Rate))


print(tabela_medias, n = Inf)




df_tabela_medias <- as.data.frame(tabela_medias)

df_tabela_medias <- df_tabela_medias %>%
  mutate(across(where(is.numeric), ~format(.x, nsmall = 1)))










sort_profile <- final_data %>%
  arrange(desc(Perfil))


all_data <- all_data %>%
  arrange(all_data[[1]]) 

all_data <- all_data %>%
  rename(Perfil = 1) %>%  # Renomeia a primeira coluna para 'Perfil'
  arrange(Perfil)


rm(list=ls())

# Cálculo de métricas
calculator <- MetricsCalculator$new(final_data)
medias <- calculator$calcular_medias()
variacoes <- calculator$calcular_variacoes()

# Geração de visualizações
visualizer <- DataVisualizer$new(all_data)
p1 <- visualizer$plot_followers_growth()
p2 <- visualizer$plot_engagement_rate()
p3 <- visualizer$plot_avg_likes_vs_comments()

# Exibição dos gráficos
print(p1)
print(p2)
print(p3)

# Exportação dos dados processados
exporter <- DataExporter$new(all_data)
exporter$export_to_csv()
exporter$export_to_excel()
exporter$export_to_rds()

message("Processamento concluído com sucesso!")
