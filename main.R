library(dplyr)
library(tidyverse)
library(R6)
source("data_processing.R")
source("calculations.R")
source("visualizations.R")
source("export.R")

# Definir caminho do arquivo de entrada
file_path <- "C:/Users/Erlon/OneDrive/dataLab/CORRIDA_GOV_2026/raspagem25.txt"
processor <- DataProcessor$new(file_path)
dados <- processor$data
dados <- dados %>%
  mutate(SUM.AVG.Engajament = rowSums(select(., AVG.Likes, AVG.Comments), na.rm = TRUE))

dados <- dados %>%
  arrange(desc(SUM.AVG.Engajament))

dados

colunas_desejadas <- dados %>%
  select(Followers, Engagement.Rate,AVG.Likes, AVG.Comments,SUM.AVG.Engajament)

dados <- dados[, -1]

rk_engagement <- dados %>%
  arrange(desc(Engagement.Rate))

rk_followers <- dados %>%
  arrange(desc(Followers))

rk_avgLikes <- dados %>%
  arrange(desc(AVG.Likes))

rk_avgComments <- colunas_desejadas %>%
  arrange(desc(AVG.Comments))

rk_media <- dados %>%
  arrange(desc(Media.Uploads))

dados <- dados %>%
  mutate(SUM.AVG.Engajament = rowSums(select(., AVG.Likes, AVG.Comments), na.rm = TRUE))

rm(file_path,processor,dados,dados,raw_lines,current_datetime,current_profile,profile_names,line)
rm(final_data,profile_data,profiles_data)
rm(list=ls())

# Calcular métricas
calculator <- MetricsCalculator$new(dados)
medias <- calculator$calcular_medias()
variacoes <- calculator$calcular_variacoes()

# Gerar visualizações
visualizer <- DataVisualizer$new(dados)
p1 <- visualizer$plot_followers_growth()
p2 <- visualizer$plot_engagement_rate()
p3 <- visualizer$plot_avg_likes_vs_comments()

# Exibir gráficos
print(p1)
print(p2)
print(p3)

# Exportar dados
exporter <- DataExporter$new(dados)
exporter$export_to_csv()
exporter$export_to_excel()
exporter$export_to_rds()

message("Processamento concluído com sucesso!")

