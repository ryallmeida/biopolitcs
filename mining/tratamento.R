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
               fs,
               stringi,
               viridis, 
               rio, 
               purrr,
               foreign,
               devtools,
               remotes)

# FONTE DOS DADOS: SISTEMA DE INFORMAÇÃO E AGRAVOS DE NOTIFICAÇÃO; IPEA DATA E DADOS ABERTOS DA CAMARA DOS DEPUTADOS

# ==============================================================================
# CARREGANDO DADOS
# ==============================================================================

# pasta onde estão TODOS os csv

folder <- "C:/Users/ryall/Desktop/DDS/DADOS/Propositions"

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

# INFORMAÇÕES DE INTERESSE --------------------------------------------------------------------------------

dados <- combined

df_filtrado <- dados %>%
  filter(
    if_any(
      everything(),
      ~ str_detect(
        as.character(.x),

        regex("aids|hiv|profilaxia pre-exposição|profilaxia pre exposição|prep",
=======
        regex("aids|síndrome da imunodeficiência adquirida|sindrome da imunodeficiencia adquirida|síndrome da imunodeficiência|sindrome da imunodeficiencia|imunodeficiência adquirida|imunodeficiencia adquirida|doença aids|doenca aids|aidis|aidsi|aid's|hiv|vírus da imunodeficiência humana|virus da imunodeficiencia humana|imunodeficiência humana|imunodeficiencia humana|virus hiv|vírus hiv|vih|vi h|hvi|hiv1|hiv-1|hiv2|hiv-2|prep|prép|pr e p|pré-exposição|pre-exposição|preexposição|pré exposição|pre exposicao|profilaxia pré-exposição|profilaxia pre-exposição|profilaxia preexposição|profilaxia pré exposição|profilaxia pre exposicao|profilaxia pre exposição|profilaxia pré-exposicao|profilaxia para pre-exposição|profilaxia antes da exposição|profilaxia prévia à exposição|profilaxia prévia a exposição|medicação preventiva hiv|medicacao preventiva hiv|proflaxia pre-exposição|exposição ao risco|exposicao ao risco|risco de hiv|prevenção combinada|prevencao combinada"
,
              ignore_case = TRUE)
      )
    )
  )

colunas_com_termos <- dados %>%
  summarise(across(everything(), ~ any(str_detect(
    as.character(.x),
    regex("aids|hiv|profilaxia pre-exposição|profilaxia pre exposição|prep",

# pep|pép|p e p|pós-exposição|pos-exposição|posexposicao|pos exposição|profilaxia pós-exposição|profilaxia pos-exposição|profilaxia pós exposicao|profilaxia pos exposicao|profilaxia pos exposição|profilaxia pós exposição|proflaxia pós-exposição|profilaxia|profilático|profilatico|antirretroviral|antiretroviral|tratamento antirretroviral|tarv|tratamento arv|arv|

# dicionário para um futuro aprimoramento do artigo

colunas_com_termos <- dados %>%
  summarise(across(everything(), ~ any(str_detect(
    as.character(.x),
    regex("aids|síndrome da imunodeficiência adquirida|sindrome da imunodeficiencia adquirida|síndrome da imunodeficiência|sindrome da imunodeficiencia|imunodeficiência adquirida|imunodeficiencia adquirida|doença aids|doenca aids|aidis|aidsi|aid's|hiv|vírus da imunodeficiência humana|virus da imunodeficiencia humana|imunodeficiência humana|imunodeficiencia humana|virus hiv|vírus hiv|vih|vi h|hvi|hiv1|hiv-1|hiv2|hiv-2|prep|prép|pr e p|pré-exposição|pre-exposição|preexposição|pré exposição|pre exposicao|profilaxia pré-exposição|profilaxia pre-exposição|profilaxia preexposição|profilaxia pré exposição|profilaxia pre exposicao|profilaxia pre exposição|profilaxia pré-exposicao|profilaxia para pre-exposição|profilaxia antes da exposição|profilaxia prévia à exposição|profilaxia prévia a exposição|medicação preventiva hiv|medicacao preventiva hiv|proflaxia pre-exposição|exposição ao risco|exposicao ao risco|risco de hiv|prevenção combinada|prevencao combinada",
          ignore_case = TRUE)
  )))) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "contém_termo") %>%
  filter(contém_termo) %>%
  pull(coluna)

print(colunas_com_termos)
# [1] "ementa"          "ementaDetalhada" "keywords"   

# [1] "descricaoTipo"         "ementa"                "ementaDetalhada"      
# [4] "keywords"              "ultimoStatus_despacho" 

# write.csv(df_filtrado, "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/camaradata.csv", row.names = FALSE)


# =============================================================================
# CONTAGEM DE PROPOSIÇÕES COM AS INFORMAÇÕES ACIMAS LISTADAS
# =============================================================================

dict_hiv <- c(
  "hiv", "vírus da imunodeficiência humana", "virus da imunodeficiencia humana",
  "imunodeficiência humana", "imunodeficiencia humana", "virus hiv", "vírus hiv",
  "vih", "vi h", "hvi", "hiv1", "hiv-1", "hiv2", "hiv-2"
)

dict_aids <- c(
  "aids", "síndrome da imunodeficiência adquirida", "sindrome da imunodeficiencia adquirida",
  "síndrome da imunodeficiência", "sindrome da imunodeficiencia",
  "imunodeficiência adquirida", "imunodeficiencia adquirida",
  "doença aids", "doenca aids", "aidis", "aidsi", "aid's"
)

dict_prep <- c(
  "prep", "prép", "pr e p", "pré-exposição", "pre-exposição", "preexposição",
  "pré exposição", "pre exposicao", "profilaxia pré-exposição",
  "profilaxia pre-exposição", "profilaxia preexposição",
  "profilaxia pré exposição", "profilaxia pre exposicao",
  "profilaxia pré-exposicao", "profilaxia para pre-exposição",
  "proflaxia pre-exposição", "profilaxia antes da exposição",
  "profilaxia prévia à exposição", "profilaxia prévia a exposição",
  "medicação preventiva hiv", "medicacao preventiva hiv"
)

# -----------------------------
# TRANSFORMAR DICIONÁRIOS EM REGEX ÚNICO
#     - ignora acentos
#     - ignora maiúsc/minúsc
# -----------------------------

make_regex <- function(vec) {
  escaped <- str_replace_all(vec, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1") # escapa caracteres
  paste0(escaped, collapse = "|")
}

regex_hiv  <- make_regex(dict_hiv)
regex_aids <- make_regex(dict_aids)
regex_prep <- paste0(
  "\\bprep\\b",                        
  "|profilaxia\\s+pre[- ]?exposicao",  
  "|profilaxia\\s+pre[- ]?exposição",  
  "|profilaxia\\s+pre[- ]?exposi[cç][aã]o",
  "|pre[- ]?exposi[cç][aã]o",          
  "|medicacao preventiva hiv",
  "|medicação preventiva hiv"
)

# -----------------------------
# 3. CRIAR AS VARIÁVEIS DE CONTAGEM
# -----------------------------

df <- df_filtrado %>%
  mutate(
    # 1. criar um campo unificado com todas as colunas relevantes
    texto_completo = paste(
      descricaoTipo,
      ementa,
      ementaDetalhada,
      keywords,
      ultimoStatus_despacho,
      sep = " "
    ),
    
    # 2. remover acentos
    texto_noacc = stringi::stri_trans_general(texto_completo, 
                                              "Latin-ASCII"),
    
    # 3. gerar as contagens
    qtd_hiv  = str_count(tolower(texto_noacc), 
                         regex(regex_hiv,  
                               ignore_case = TRUE)),
    qtd_aids = str_count(tolower(texto_noacc), 
                         regex(regex_aids, 
                               ignore_case = TRUE)),
    qtd_prep = str_count(tolower(texto_noacc), 
                         regex(regex_prep, 
                               ignore_case = TRUE))
  ) %>%
  select(-texto_noacc)   # opcional: remove coluna auxiliar


# =============================================================================


# seus dicionários (use os originais)
dict_prep_raw <- c(
  "prep", "prép", "pr e p", "pré-exposição", "pre-exposição", "preexposição",
  "pré exposição", "pre exposicao", "profilaxia pré-exposição",
  "profilaxia pre-exposição", "profilaxia preexposição",
  "profilaxia pré exposição", "profilaxia pre exposicao",
  "profilaxia pré-exposicao", "profilaxia para pre-exposição",
  "proflaxia pre-exposição", "profilaxia antes da exposição",
  "profilaxia prévia à exposição", "profilaxia prévia a exposição",
  "medicação preventiva hiv", "medicacao preventiva hiv"
)

# função generalizada mais segura
make_pattern_safe <- function(entry) {
  e <- stringi::stri_trans_general(entry, "Latin-ASCII")
  e <- tolower(str_trim(e))
  # escapar metacaracteres (exceto espaço e hífen, que tratamos depois)
  esc <- str_replace_all(e, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  # transformar hífen em '[-\s]+' e espaços em '\s+'
  esc <- str_replace_all(esc, "-", "[-\\\\s]+")
  esc <- str_replace_all(esc, "\\s+", "\\\\s+")
  # envolver em non-capturing group e aplicar fronteiras de palavra
  # isso evita casar dentro de palavras maiores (ex: preparador)
  paste0("\\b(?:", esc, ")\\b")
}

# gerar regex final
patterns_prep <- vapply(dict_prep_raw, make_pattern_safe, FUN.VALUE = character(1))
regex_prep <- paste0(patterns_prep, collapse = "|")

# repita para HIV/AIDS (exemplo rápido)
dict_hiv_raw <- c(
  "hiv", "virus da imunodeficiencia humana", "imunodeficiencia humana",
  "virus hiv", "vih", "hiv1", "hiv-1", "hiv2", "hiv-2", "hvi", "vi h"
)
patterns_hiv <- vapply(dict_hiv_raw, make_pattern_safe, FUN.VALUE = character(1))
regex_hiv <- paste0(patterns_hiv, collapse = "|")

dict_aids_raw <- c(
  "aids", "sindrome da imunodeficiencia adquirida", "sindrome da imunodeficiencia",
  "imunodeficiencia adquirida", "doenca aids", "aidis", "aidsi", "aid's"
)

patterns_aids <- vapply(dict_aids_raw, 
                        make_pattern_safe, 
                        FUN.VALUE = character(1))

regex_aids <- paste0(patterns_aids, 
                     collapse = "|")

# recalcular no dataframe unindo colunas (protege NA com coalesce)
df3 <- df_filtrado %>%
  mutate(
    texto_completo = paste(
      coalesce(descricaoTipo, ""),
      coalesce(ementa, ""),
      coalesce(ementaDetalhada, ""),
      coalesce(keywords, ""),
      coalesce(ultimoStatus_despacho, ""),
      sep = " "
    ),
    texto_noacc = stringi::stri_trans_general(texto_completo, "Latin-ASCII"),
    texto_noacc = tolower(texto_noacc),
    
    qtd_hiv  = str_count(texto_noacc, regex(regex_hiv, ignore_case = TRUE)),
    qtd_aids = str_count(texto_noacc, regex(regex_aids, ignore_case = TRUE)),
    qtd_prep = str_count(texto_noacc, regex(regex_prep, ignore_case = TRUE))
  ) %>%
  select(-texto_noacc)

# =============================================================================
# PLOTTAGEM DA SERIE HISTÓRICA
# =============================================================================

df_plot <- df3 %>%
  mutate(
    data = as.Date(substr(dataApresentacao, 1, 10)),
    ano  = year(data)
  )


df_long <- df_plot %>%
  pivot_longer(
    cols = c(qtd_hiv, qtd_aids, qtd_prep),
    names_to = "variavel",
    values_to = "quantidade"
  ) %>%
  group_by(ano, 
           variavel) %>%
  summarise(quantidade = sum(quantidade, 
                             na.rm = TRUE), 
            .groups = "drop")

# write.csv(df_long, "C:/Users/ryall/Desktop/DDS/DADOS/assuntoparlamentar.csv")

p1 <- ggplot(df_long, aes(x = ano, 
                          y = quantidade, 
                          fill = variavel)) +
  
  annotate("rect",
           xmin = 2017, 
           xmax = 2025,
           ymin = -Inf, 
           ymax = Inf,
           alpha = 0.5, 
           fill = "grey80") +
  
  geom_col(position = "stack", width = 0.75) +
  
  scale_fill_viridis_d(
    option = "cividis",
    end = 0.9,
    labels = c(
      qtd_aids = "Aids",
      qtd_hiv  = "HIV",
      qtd_prep = "PrEP"
    )
  ) +
  
  scale_x_continuous(breaks = seq(min(df_long$ano), 
                                  max(df_long$ano), by = 1)) +
  
  labs(
    title = "",
    x = "Ano",
    y = "Quantidade de menções",
    fill = "Assunto parlamentar"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank())

print(p1)

## ggsave("C:/Users/ryall/Downloads/citacoes_camera.png", plot = p1, width = 10, height = 6, dpi = 300)

