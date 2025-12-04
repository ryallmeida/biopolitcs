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
               viridis)

# TRATAMENTO NOS MICRODADOS DO PAINEL PREP, DISPONÍVEL EM:
# https://www.gov.br/aids/pt-br/indicadores-epidemiologicos/painel-de-monitoramento/painel-prep

dispensas <- read.csv("C:/Users/ryall/Downloads/Dados_PrEP_transparencia/Banco_PrEP_dispensas.csv")

# OS BANCO DE DADOS SÃO EQUIVALENTES: usuarios = dados

usuarios <- read.csv("C:/Users/ryall/Downloads/Dados_PrEP_transparencia/Banco_PrEP_usuarios.csv")
dados <- read.csv("https://raw.githubusercontent.com/ryallmeida/biopolitcs/refs/heads/main/dataframes/df_em_prep.csv")

#   QUERO CONTA A VOLUMETRIA TOTAL DE OBSERVAÇÕES DE  USUÁRIOS EM PREP

# identificar automaticamente todas as colunas referentes a anos de PrEP
colunas_prep <- grep("^EmPrEP_", 
                     names(dados), 
                     value = TRUE)

# criar um vetor vazio para guardar resultados
contagem_por_ano <- numeric(length(colunas_prep))

# loop para contar por ano
# conta quantas observações têm "Em PrEP" na coluna daquele ano

for(i in seq_along(colunas_prep)){
  coluna <- colunas_prep[i]
  contagem_por_ano[i] <- sum(grepl("Em PrEP", dados[[coluna]], ignore.case = TRUE), na.rm = TRUE)
}

# criar data.frame bonitinho
em_prep <- data.frame(
  Ano = gsub("EmPrEP_", "", colunas_prep),
  Total_EmPrEP = contagem_por_ano
)

print(em_prep)

#write.csv(dados, "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/df_em_prep.csv")

##write.csv(em_prep, "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/contagem_prep.csv")

# =============================================================================
# MINERAÇÃO DE DADOS 1
# =============================================================================


long <- usuarios |>
  pivot_longer(
    cols = starts_with("EmPrEP_"),
    names_to = "ano",
    values_to = "status"
  ) |>
  mutate(
    ano = str_extract(ano, "\\d{4}") |> as.integer(),
    status = case_when(
      str_detect(status, "Em PrEP") ~ "Em_PrEP",
      str_detect(status, "Descontinuou") ~ "Descontinuou",
      status == "" ~ "Vazio",
      TRUE ~ status
    )
  )

proporcoes <- long |>
  group_by(ano, status) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(prop = 100 * n / sum(n)) |>
  ungroup()

ggplot(proporcoes, aes(
  x = prop,
  y = factor(ano),
  fill = status
)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "cividis") +  
  labs(
    x = "Proporção (%)",
    y = "Ano",
    fill = "Status"
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 14)

# =============================================================================
# MINERAÇÃO DE DADOS 2
# =============================================================================

# --- Ajustes iniciais ---
# defina os anos que existem nas colunas
anos <- 2018:2024
cols <- paste0("EmPrEP_", anos)

# -------------------------------------------------
# A) Se cada LINHA é um indivíduo 
# -------------------------------------------------
dados_long <- usuarios %>%
  # garante um id por linha (se já houver coluna ID, substitua)
  mutate(.row_id = row_number()) %>%
  # seleciona só as colunas-year (ajuste se tiver outras colunas)
  select(.row_id, all_of(cols)) %>%
  pivot_longer(-.row_id, names_to = "ano", values_to = "raw_status") %>%
  mutate(
    ano = as.integer(str_extract(ano, "\\d{4}")),
    # normaliza o texto para categorias
    status = case_when(
      str_detect(raw_status, regex("Em PrEP", ignore_case = TRUE)) ~ "Em_PrEP",
      str_detect(raw_status, regex("Descontinuou", ignore_case = TRUE)) ~ "Descontinuou",
      # trata strings vazias como NA (removemos essa categoria do cálculo)
      str_trim(raw_status) == "" | is.na(raw_status) ~ NA_character_,
      TRUE ~ NA_character_  # qualquer outro texto vira NA para segurança
    )
  )

# -------------------------------------------------
# Detectar "Voltou": marcar a PRIMEIRA vez que a pessoa está Em_PrEP
# depois de ter tido ao menos um "Descontinuou" em anos anteriores.
# -------------------------------------------------

dados_com_volta <- dados_long %>%
  group_by(.row_id) %>%
  arrange(ano) %>%
  group_modify(~ {
    df <- .x
    # posições em que foi "Descontinuou"
    pos_dis <- which(df$status == "Descontinuou")
    if (length(pos_dis) == 0) {
      return(df) # sem descontinuação -> nada a marcar
    }
    earliest_dis <- min(pos_dis)
    # procurar a primeira Em_PrEP depois desta posição
    pos_return <- which(df$status == "Em_PrEP" & seq_len(nrow(df)) > earliest_dis)
    if (length(pos_return) > 0) {
      first_return <- pos_return[1]
      df$status[first_return] <- "Voltou"   # substitui "Em_PrEP" por "Voltou" naquele ano
    }
    df
  }) %>%
  ungroup()

# -------------------------------------------------
# Calcular proporções por ano EXCLUINDO NAs (=vazios)
# -------------------------------------------------
proporcoes <- dados_com_volta %>%
  filter(!is.na(status)) %>%         # exclui células vazias/NA do denominador
  group_by(ano, status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ano) %>%
  mutate(prop = 100 * n / sum(n)) %>%
  ungroup()

proporcoes$status <- factor(
  proporcoes$status,
  levels = c("Em_PrEP", "Voltou", "Descontinuou")
)

p1 <- ggplot(proporcoes, aes(
  x = prop,
  y = factor(ano, levels = sort(unique(ano), decreasing = TRUE)),
  fill = status
)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = NA,
    width = 0.6     # afina a barra
  ) +
  scale_fill_viridis_d(
    option = "rocket",  # escala estética ótimo→ruim
    direction = -1,
    begin = 0.15,
    end = 0.85,
    name = "Situação",
    labels = c(
      "Em_PrEP" = "Em PrEP",
      "Voltou" = "Retorno à PrEP",
      "Descontinuou" = "Descontinuou"
    )
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    x = "Proporção (%)",
    y = "Ano"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


print(p1)


p2 <- ggplot(proporcoes, aes(
  x = prop,
  y = factor(ano, levels = sort(unique(ano), decreasing = TRUE)),
  fill = status
)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = NA,
    width = 0.6
  ) +
  geom_text(
    aes(
      label = paste0(round(prop, 1), "%")
    ),
    position = position_stack(vjust = 0.5),  # centra o texto na parte da barra
    color = "white",
    size = 3.3,
    fontface = "bold"
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1,
    begin = 0.15,
    end = 0.85,
    name = "Situação",
    labels = c(
      "Em_PrEP" = "Em PrEP",
      "Voltou" = "Retorno à PrEP",
      "Descontinuou" = "Descontinuou"
    )
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 100)) +
  labs(
    x = "Proporção (%)",
    y = "Ano"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p2)

# CRIAÇÃO DO ESPECTRO

# Supondo que proporcoes tem status e ano
# Vamos criar "score" para cada status
# Escala: 0 = péssimo (Descontinuou e não voltou), 0.5 = voltou, 1 = Em PrEP

proporcoes <- proporcoes %>%
  mutate(
    score = case_when(
      status == "Em_PrEP" ~ 1,
      status == "Voltou" ~ 0.5,
      status == "Descontinuou" ~ 0,
      TRUE ~ NA_real_
    )
  )

p3 <- ggplot(proporcoes, aes(
  x = prop,
  y = factor(ano, levels = sort(unique(ano), decreasing = TRUE)),
  fill = score
)) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  scale_fill_viridis_c(
    option = "cividis",   # paleta contínua cividis
    direction = 1,
    begin = 0.15,
    end = 0.85,
    name = "Adesão à PrEP"
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) +
  labs(
    x = "Proporção (%)",
    y = "Ano"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p3)