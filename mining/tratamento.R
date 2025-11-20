# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# LABORATÓRIO DE ESTUDO DA SEXUALIDADE HUMANA (LABESHU)
# DEPARTAMENTO DE PSICOLOGIA

# --------------------------------
# CODADO ORIGINALMENTE EM R, VERSÃO 4.5.2
# --------------------------------
# DE AUTORIA DE RYAN ALMEIDA

if(!require("pacman")){
  install.packages("pacman")
}

pacman::p_load(tidyverse,
               fs)
 
# ==============================================================================
# CARREGANDO DADOS
# ==============================================================================

# pasta onde estão TODOS os csv
folder <- "C:/Users/ryall/Downloads/Propositions"

# lista de arquivos csv
files <- fs::dir_ls(folder, 
                    regexp = "\\.csv$", 
                    ignore.case = TRUE)

# função para ler um csv e marcar origem
read_with_source <- function(path) {
  df <- readr::read_delim(
    file = path,
    delim = ";",
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  df %>%
    mutate(
      source_file = path_file(path),
      source_row  = row_number(),
      source_id   = paste0(source_file, "_", source_row)
    )
}

# lê todos os csv e combina preservando todas as observações
combined <- map_dfr(files, 
                    read_with_source)

# extrai ANO dos nomes dos arquivos (primeiro grupo de 4 dígitos)
combined <- combined %>%
  mutate(
    year = stringr::str_extract(source_file, "\\d{4}"),
    year = as.integer(year)
  )

# salvar o resultado

# write.csv(combined, "C:/Users/ryall/Desktop/R/biopolitica/dataframes/camaradata.csv", row.names = FALSE)

combined



# INFORMAÇÕES DE INTERESSE --------------------------------------------------------------------------------

dados_2013 <- read.csv("C:/Users/ryall/Downloads/proposicoes-2013.csv", 
                  sep = ";")

df_filtrado <- dados %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(
        as.character(.x),
        regex("aids|hiv|profilaxia pre-exposição|profilaxia pre exposição|prep",
              ignore_case = TRUE)
      )
    )
  )

colunas_com_termos <- dados %>%
  summarise(across(everything(), ~ any(str_detect(
    as.character(.x),
    regex("aids|hiv|profilaxia pre-exposição|profilaxia pre exposição|prep",
          ignore_case = TRUE)
  )))) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "contém_termo") %>%
  filter(contém_termo) %>%
  pull(coluna)

print(colunas_com_termos)
# [1] "ementa"          "ementaDetalhada" "keywords"   


