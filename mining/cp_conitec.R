# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# LABORATÓRIO DE ESTUDO DA SEXUALIDADE HUMANA (LABESHU)
# DEPARTAMENTO DE PSICOLOGIA

# --------------------------------
# CODADO ORIGINALMENTE EM R, VERSÃO 4.5.2
# --------------------------------
# DE AUTORIA DE RYAN ALMEIDA

if(!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse,
               pdftools,
               lubridate,
               viridis)

dados <- backup


# ==============================================================================


arquivo_pdf <-"C:/Users/ryall/Downloads/consultas_públicas_conitec/cp_conitec_02_2017_pcdt_fibrose_cistica__insuficiencia_pancreatica.pdf"

texto_paginas <- pdf_text(arquivo_pdf)

dados_brutos <- tibble(
  pagina = seq_along(texto_paginas),
  texto = texto_paginas
)

dados_linhas <- dados_brutos %>%
  unnest_longer(
    texto,
    values_to = "linha",
    keep_empty = FALSE
  ) %>%
  separate_rows(linha, sep = "\n")

dados_linhas <- dados_linhas %>%
  mutate(
    inicio_contrib = str_detect(linha, "^\\d{2}/\\d{2}/\\d{4}")
  )

dados_linhas <- dados_linhas %>%
  mutate(
    id_contrib = cumsum(inicio_contrib)
  ) %>%
  filter(id_contrib > 0)

dados_contrib <- dados_linhas %>%
  group_by(id_contrib) %>%
  summarise(
    texto_completo = str_squish(paste(linha, collapse = " ")),
    .groups = "drop"
  )

dados_contrib <- dados_contrib %>%
  mutate(
    data = str_extract(texto_completo, "\\d{2}/\\d{2}/\\d{4}"),
    perfil = str_extract(
      texto_completo,
      "Profissional de saúde|Paciente|Interessado no tema|Familiar, amigo ou cuidador de paciente|Sociedade médica"
    )
  )

# ==============================================================================
# NOTAS
# ==============================================================================

# CONSULTAS PÚBLICAS DE Nºº 30, 32, 37 E 64 NÃO HOUVERAM CONTRIBUIÇÕES

# ==============================================================================
# MINERAÇÃO DE DADOS A PARTIR DOS PDFS
# ==============================================================================

pasta <- "C:/Users/ryall/Downloads/consultas_públicas_conitec"

arquivos_pdf <- list.files(
  path = pasta,
  pattern = "\\.pdf$",
  full.names = TRUE
)

extrair_consulta_publica <- function(caminho_arquivo) {
  as.integer(
    str_extract(
      basename(caminho_arquivo),
      "(?<=cp_conitec_)\\d{2}(?=_2017)"
    )
  )
}

extrair_campos <- function(txt) {
  
  tibble(
    data = str_extract(txt, "\\d{2}/\\d{2}/\\d{4}"),
    
    perfil = str_extract(
      txt,
      "Paciente|Profissional de saúde|Interessado no tema|Interessado no tema|Familiar, amigo ou cuidador de paciente|Sociedade médica"
    ),
    
    avaliacao = str_extract(
      txt,
      "Concordo totalmente com a recomendação preliminar|
       Concordo parcialmente da recomendação preliminar|
       Discordo da recomendação preliminar"
    ),
    
    sugestao_texto = if_else(
      str_detect(txt, "Sim,"),
      str_trim(str_extract(txt, "Sim,.*")),
      NA_character_
    ),
    
    comentario_extra = case_when(
      str_detect(txt, "Gostaria de comentar") ~
        str_trim(str_extract(txt, "Gostaria de comentar.*")),
      TRUE ~ NA_character_
    ),
    
    texto_integral = txt
  )
}


processar_pdf <- function(arquivo_pdf) {
  
  # ---- número da consulta pública
  consulta_publica <- extrair_consulta_publica(arquivo_pdf)
  
  # ---- leitura do PDF
  texto <- pdf_text(arquivo_pdf)
  
  linhas <- tibble(
    linha = unlist(str_split(texto, "\n"))
  ) %>%
    mutate(linha = str_squish(linha)) %>%
    filter(linha != "")
  
  linhas <- linhas %>%
    mutate(
      nova_contrib = str_detect(linha, "^\\d{2}/\\d{2}/\\d{4}")
    ) %>%
    mutate(id = cumsum(nova_contrib)) %>%
    filter(id > 0)
  
  contrib_blocos <- linhas %>%
    group_by(id) %>%
    summarise(
      texto = str_squish(paste(linha, collapse = " ")),
      .groups = "drop"
    )
  
  dados_final <- contrib_blocos %>%
    mutate(extraido = map(texto, extrair_campos)) %>%
    unnest(extraido) %>%
    mutate(
      consulta_publica = consulta_publica,
      arquivo_origem = basename(arquivo_pdf)
    )
  
  return(dados_final)
}

dados <- map_dfr(arquivos_pdf, processar_pdf)


# ==============================================================================
# MINERAÇÃO DE DADOS: TIPOS DE CONTRIBUINTES
# ==============================================================================

dados <- dados %>%
  mutate(
    texto_inicio = str_to_lower(str_sub(texto_integral, 1, 50)),
    
    contribuinte = case_when(
      
      str_detect(texto_inicio, "empresa fabricante|fabricante da tecnologia") ~
        "Empresa fabricante da tecnologia avaliada",
      
      str_detect(texto_inicio, "\\bempresa\\b") ~
        "Empresa",
      
      str_detect(texto_inicio, "organiza.*sociedade civil|osc\\b|ong\\b") ~
        "Organização da Sociedade Civil",
      
      str_detect(texto_inicio, "institui.*ensino|universidad|faculdade") ~
        "Instituição de ensino",
      
      str_detect(texto_inicio, "institui.*sa[uú]de|hospital|cl[ií]nica") ~
        "Instituição de saúde",
      
      str_detect(texto_inicio, "especialist") ~
        "Especialista no tema do protocolo",
      
      str_detect(texto_inicio, "profissional.*sa[uú]de|m[eé]dico|enfermeir") ~
        "Profissional de saúde",
      
      str_detect(texto_inicio, "\\bpaciente\\b") ~
        "Paciente",
      
      str_detect(texto_inicio, "familiar|cuidador|amigo") ~
        "Familiar, amigo ou cuidador de paciente",
      
      TRUE ~ "Não identificado"
    )
  ) 


# ==============================================================================
# PRIMEIRO CHECKPOINT
# ==============================================================================

readr::write_csv(
  dados,
  "C:/Users/ryall/Downloads/dados_conitec.csv",
  na = "",
  quote = "needed"
)

dados <- readr::read_csv(
  "C:/Users/ryall/Downloads/dados_conitec.csv",,
  show_col_types = FALSE
)

# ==============================================================================
# ORGANIZAR MELHOR A VARIAVEL: TEXTO INTEGRAL [REMOÇÃO DE RUÍDOS]
# ==============================================================================

    
dados <- dados %>%
  mutate(text_tolower = str_to_lower(str_sub(texto_integral, 1, 70)), 
         
    concordancia = case_when(
      
      str_detect(text_tolower, "discordo totalmente") ~
        "Discordo totalmente da recomendação preliminar",
      
      str_detect(text_tolower, "discordo parcialmente") ~
        "Discordo parcialmente da recomendação preliminar",
      
      str_detect(text_tolower, "concordo totalmente") ~
        "Concordo totalmente da recomendação preliminar",
      
      str_detect(text_tolower, "concordo parcialmente") ~
        "Concordo parcialmente da recomendação preliminar",
      
      TRUE ~ "Não identificado"
    )
  ) 

dados <- dados %>%
  dplyr::select(-text_tolower, -texto_inicio)

dados_filtrados <- dados %>%
  dplyr::filter(stringr::str_detect(concordancia, "Não identificado"))
# REALMENTE LENDO NOMINALMENTE ESSAS OBSERVAÇÕES, OS DADOS NAO FAZEM MUITO SENTIDO.

# ==============================================================================
# COLOCAR AS DATAS DE INICIO E TERMINO DA CONSULTA PÚBLICA NO BANCO DE DADOS
# ==============================================================================

dados_consultas <- data.frame(
  consulta = 1:69,
  inicio = c(
    "20012017","23022017","23022017","23022017","23022017",
    "25032017","25032017","25032017","25032017","25032017",
    "25032017","08/04/2017","12042017","12042017","12042017",
    "13042027","25042017","25042017","25042017","25042017",
    "25042017","25042017","13052017","13052017","25052017",
    "26072017","26072017","26072017","15072017","20072017",
    "20072017","24072017","04/08/2017","04/08/2017","04/08/2017",
    "04/08/2017", NA,"17082017","17082017","17082017",
    "17082017","17082017","17082017","11092017","11092017",
    "11092017","26092017","26092017","26092017","26092017",
    "26092017","20102017","25102017","30102017","25102017",
    "20102017","25102017","25102017","25102017","25102017",
    "29112017","29112017","29112017","29112017","29112017",
    "29112017","29112017","21122017","21122017"
  ),
  termino = c(
    "08022017","14032017","12032017","14032017","14032017",
    "13042017","13042017","13042017","13042017","13042017",
    "13042017","27042017","02/05/2017","02/05/2017","02/05/2017",
    "02/05/2017","16052017","16052017","16052017","16052017",
    "16052017","16052017","01/06/2017","01/06/2017","15062017",
    "06/07/2017","06/07/2017","06/07/2017","03/08/2017","08/08/2017",
    "08/08/2017","14082017","23082017","23082017","23082017",
    "23082017", NA,"05/09/2017","05/09/2017","05/09/2017",
    "05/09/2017","05/09/2017","05/09/2017","02/10/2017","02/10/2017",
    "02/10/2017","17102017","17102017","17102017","17102017",
    "17102017","13112017","13112017","20112017","13112017",
    "13112017","13112017","13112017","13112017","13112017",
    "18122017","18122017","18122017","18122017","26122017",
    "18122017","18122017","19022018","19022018"
  ),
  stringsAsFactors = FALSE
)

dados_consultas$inicio  <- lubridate::dmy(dados_consultas$inicio)
dados_consultas$termino <- lubridate::dmy(dados_consultas$termino)

dados <- dados %>%
  left_join(
    dados_consultas,
    by = c("consulta_publica" = "consulta")
  )

dados$data <- lubridate::dmy(dados$data)

# ==============================================================================
# SEGUNDO CHECKPOINT
# ==============================================================================

readr::write_csv(
  dados,
  "C:/Users/ryall/Downloads/dados_conitec.csv",
  na = "",
  quote = "needed"
)

dados <- readr::read_csv(
  "C:/Users/ryall/Downloads/dados_conitec.csv",
  show_col_types = FALSE
)

# ==============================================================================
# PLOT DA PROXY DE OPINIÃO PÚBLICA: DISTRIBUIÇÃO TEMPORAL DOS EVENTOS
# ==============================================================================
interesse <- as.data.frame(table(dados$consulta_publica))

ggplot(dados, aes(x = data)) +
  geom_histogram(
    binwidth = 7, 
    boundary = as.numeric(as.Date("2000-01-01")),
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%Y"
  ) +
  labs(
    x = "Tempo",
    y = "Número de eventos",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ==============================================================================
# PLOT DA PROXY DE OPINIÃO PÚBLICA: DISTRIBUIÇÃO TEMPORAL DOS EVENTOS COM AS PROPORÇÕES DE APROVAÇÃO
# ==============================================================================

df_mensal$concordancia <- factor(
  df_mensal$concordancia,
  levels = c(
    "Concordo totalmente da recomendação preliminar",
    "Concordo parcialmente da recomendação preliminar",
    "Não identificado",
    "Discordo parcialmente da recomendação preliminar",
    "Discordo totalmente da recomendação preliminar"
  )
)


dados$mes <- lubridate::floor_date(dados$data, unit = "month")
dados$semana <- lubridate::floor_date(dados$data, unit = "week")

df_mensal <- dados %>%
  count(semana, concordancia) %>%
  group_by(semana) %>%
  mutate(
    prop = n / sum(n),                 # proporção
    perc = prop * 100                  # percentual
  ) %>%
  ungroup()

df_mensal <- df_mensal %>%
  mutate(
    label = ifelse(
      perc >= 10,
      paste0(round(perc, 0), "%"),
      NA
    )
  )

# ==============================================================================
# TERCEIRO CHECKPOINT
# ==============================================================================


readr::write_csv(
  df_mensal,
  "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/cp_conitec_prop.csv",
  na = "",
  quote = "needed"
)

df_mensal <- readr::read_csv(
  "https://raw.githubusercontent.com/ryallmeida/biopolitcs/refs/heads/main/dataframes/cp_conitec_prop.csv",
  show_col_types = FALSE
)

# ==============================================================================
# ==============================================================================

periodos <- data.frame(
  inicio = as.Date(c("2017-02-12", "2017-06-18", "2017-05-25", "2017-05-13")),
  fim    = as.Date(c("2017-03-19", "2017-07-09", "2017-06-15", "2017-06-01"))
)

y_max <- max(df_mensal$n, na.rm = TRUE)
periodos$y <- y_max * c(1.20, 1.10, 1.05, 1.20) 

df_mensal <- df_mensal %>% 
  dplyr::select(-label)

df_mensal <- df_mensal %>%
  mutate(
    label = ifelse(
      perc >= 35 | perc == 90,
      paste0(round(perc, 0), "%"),
      NA
    )
  )

# ==============================================================================
# ==============================================================================


ggplot(df_mensal, aes(x = semana, y = n, fill = concordancia)) +
  geom_col(width = 6) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 2.5
  ) +
  geom_rect(
    data = periodos,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.2
  ) +
  geom_text(
    data = periodos,
    aes(
      x = inicio + (fim - inicio) / 2,
      y = y,
      label = c(
        "Discussão do PCDT \n PrEP, Tenofovir associado a entricitabina", 
        "HIV", 
        "Raltegravir", 
        "PCDT, Aids em Crianças e Adolescentes"
      )
    ),
    inherit.aes = FALSE,
    vjust = 1.5,
    size = 5,
    color = "grey30"
  ) +
  scale_fill_viridis_d(option = "rocket") +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d/%m/%Y"
  ) +
  labs(
    x = "Tempo",
    y = "Índice de observações",
    fill = "Concordância",
    title = ""
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, 
                               vjust = 0.5, 
                               hjust = 0.5,
                               size = 9), 
    legend.position = "bottom",
    legend.direction = "horizontal")




