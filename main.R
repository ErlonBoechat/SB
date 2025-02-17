library(tidyverse)
library(R6)
source("data_processing.R")
source("calculations.R")
source("visualizations.R")
source("export.R")

# Definir caminho do arquivo de entrada
file_path <- "C:/Users/Erlon/OneDrive/dataLab/CORRIDA_GOV_2026/raspagem20.txt"

# Processar os dados
processor <- DataProcessor$new(file_path)

df_dados <- as.data.frame(processor$data)

processor <- NULL
df_dados <- NULL
dados <- NULL
raw_lines <- NULL
profile_names <- NULL


dados <- processor$data

df_dados <- as.data.frame(dados)

plot(df_dados$Perfil,df_dados$Media.Uploads)

help(plot)

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

