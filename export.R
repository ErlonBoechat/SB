library(tidyverse)
library(R6)

# Classe para exportação de dados
DataExporter <- R6::R6Class("DataExporter",
                            public = list(
                              data = NULL,
                              
                              initialize = function(data) {
                                if (is.null(data) || nrow(data) == 0) {
                                  stop("Os dados fornecidos estão vazios ou são inválidos.")
                                }
                                self$data <- data
                              },
                              
                              export_to_csv = function(file_path = "output/dados_processados.csv") {
                                write_csv(self$data, file_path)
                                message("Dados exportados com sucesso para ", file_path)
                              },
                              
                              export_to_excel = function(file_path = "output/dados_processados.xlsx") {
                                if (!requireNamespace("writexl", quietly = TRUE)) {
                                  stop("O pacote 'writexl' não está instalado. Instale com install.packages('writexl')")
                                }
                                writexl::write_xlsx(self$data, file_path)
                                message("Dados exportados com sucesso para ", file_path)
                              },
                              
                              export_to_rds = function(file_path = "output/dados_processados.rds") {
                                saveRDS(self$data, file_path)
                                message("Dados exportados com sucesso para ", file_path)
                              }
                            )
)
