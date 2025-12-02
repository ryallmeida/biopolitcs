# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# LABORATÓRIO DE ESTUDO DA SEXUALIDADE HUMANA (LABESHU)
# DEPARTAMENTO DE PSICOLOGIA

# --------------------------------
# CODADO ORIGINALMENTE EM R, VERSÃO 4.5.2
# --------------------------------
# DE AUTORIA DE RYAN ALMEIDA

# UTILIZANDO A API DO SENADO FEDERAL PARA MAPEAR MENSÕES E PROPOSIÇÕES QUE CITEM PREP, HIV E AIDS

if(!require("pacman")){
  install.packages("pacman")
}

pacman::p_load(tidyverse,
               fs,
               stringi,
               tidyr)

# ===========================================================================

dados <- readxl::read_excel("C:/Users/ryall/Downloads/copy2_of_pb_painel_demandas_conitec_20251112.xlsx")

# =============================================================================
# 1. FILTRAR LINHAS QUE CONTENHAM QUALQUER TERMO NA TABELA
# =============================================================================

padrao_busca <- regex(
  "aids|síndrome da imunodeficiência adquirida|sindrome da imunodeficiencia adquirida|
   síndrome da imunodeficiência|sindrome da imunodeficiencia|imunodeficiência adquirida|
   imunodeficiencia adquirida|doença aids|doenca aids|aidis|aidsi|aid's|hiv|
   vírus da imunodeficiência humana|virus da imunodeficiencia humana|
   imunodeficiência humana|imunodeficiencia humana|virus hiv|vírus hiv|vih|vi h|hvi|
   hiv1|hiv-1|hiv2|hiv-2|prep|prép|pr e p|pré-exposição|pre-exposição|preexposição|
   pré exposição|pre exposicao|profilaxia pré-exposição|profilaxia pre-exposição|
   profilaxia preexposição|profilaxia pré exposição|profilaxia pre exposicao|
   profilaxia pré-exposicao|profilaxia para pre-exposição|proflaxia pre-exposição|
   profilaxia antes da exposição|profilaxia prévia à exposição|profilaxia prévia a exposição|
   medicação preventiva hiv|medicacao preventiva hiv|exposição ao risco|
   exposicao ao risco|risco de hiv|prevenção combinada|prevencao combinada",
  ignore_case = TRUE
)

df_filtrado <- dados %>%
  filter(if_any(everything(), ~ str_detect(as.character(.x), padrao_busca)))


# =============================================================================
# 2. IDENTIFICAR COLUNAS QUE POSSUEM OS TERMOS
# =============================================================================

colunas_com_termos <- dados %>%
  summarise(across(everything(),
                   ~ any(str_detect(as.character(.x), padrao_busca)))) %>%
  pivot_longer(everything(),
               names_to = "coluna",
               values_to = "contém_termo") %>%
  filter(contém_termo) %>%
  pull(coluna)

#print(colunas_com_termos)
# [1] "Indicação"

# =============================================================================
# 3. DICIONÁRIOS PARA CONTAGEM
# =============================================================================

dict_hiv_raw <- c(
  "hiv", "virus da imunodeficiencia humana", "imunodeficiencia humana",
  "virus hiv", "vih", "hiv1", "hiv-1", "hiv2", "hiv-2", "hvi", "vi h"
)

dict_aids_raw <- c(
  "aids", "sindrome da imunodeficiencia adquirida", "sindrome da imunodeficiencia",
  "imunodeficiencia adquirida", "doenca aids", "aidis", "aidsi", "aid's"
)

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


# =============================================================================
# 4. FUNÇÃO ROBUSTA PARA CRIAR REGEXS
# =============================================================================

make_pattern_safe <- function(entry) {
  e <- stringi::stri_trans_general(entry, "Latin-ASCII")
  e <- tolower(str_trim(e))
  e <- str_replace_all(e, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  e <- str_replace_all(e, "-", "[-\\\\s]+")
  e <- str_replace_all(e, "\\s+", "\\\\s+")
  paste0("\\b(?:", e, ")\\b")
}

regex_hiv  <- paste0(vapply(dict_hiv_raw, make_pattern_safe, FUN.VALUE = character(1)), collapse = "|")
regex_aids <- paste0(vapply(dict_aids_raw, make_pattern_safe, FUN.VALUE = character(1)), collapse = "|")
regex_prep <- paste0(vapply(dict_prep_raw, make_pattern_safe, FUN.VALUE = character(1)), collapse = "|")


# =============================================================================
# 5. CONTAGEM FINAL
# =============================================================================

df3 <- df_filtrado %>%
  mutate(
    texto_completo = paste(
      coalesce(`Indicação`, ""),  
      sep = " "
    ),
    
    texto_noacc = stringi::stri_trans_general(tolower(texto_completo), "Latin-ASCII"),
    
    qtd_hiv  = str_count(texto_noacc, regex(regex_hiv, ignore_case = TRUE)),
    qtd_aids = str_count(texto_noacc, regex(regex_aids, ignore_case = TRUE)),
    qtd_prep = str_count(texto_noacc, regex(regex_prep, ignore_case = TRUE))
  ) %>%
  select(-texto_noacc)

# write.csv(df3, "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/conitec.csv", row.names = FALSE)

