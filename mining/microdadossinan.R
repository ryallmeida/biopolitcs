# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# LABORATÓRIO DE ESTUDO DA SEXUALIDADE HUMANA (LABESHU)
# DEPARTAMENTO DE PSICOLOGIA

# --------------------------------
# CODADO ORIGINALMENTE EM R, VERSÃO 4.5.2
# --------------------------------
# DE AUTORIA DE RYAN ALMEIDA

# TRATAMENTO NOS MICRODADOS DO DATASUS

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
               readxl)

install.packages("remotes")

# PARA RODAR O CODIGO ABAIXO TIVE QUE BAIXAR O RTOOLS NA VERSÃO 4.5 
remotes::install_github("rfsaldanha/read.dbc")
library(read.dbc)

# ==============================================================================
# DADOS DOS SISTEMA DO AGRAVO DE NOTIFICAÇÕES, INFORMAÇÕES SOBRE HIV E AIDS EM CRIANÇAS E ADULTOS
# ==============================================================================

populacao <- rio::import("C:/Users/ryall/Downloads/populacao_residente_estimativa.csv")


df_pop <- populacao %>%
  pivot_longer(
    cols = `1992`:`2025`,       # todas as colunas de anos
    names_to = "ano",           # nova coluna com os anos
    values_to = "populacao"     # nova coluna com a estimativa
  ) %>%
  mutate(ano = as.integer(ano)) # transforma ano em número 

# ==============================================================================
# CALCULANDO A SERIE HISTORICA DA INCIDÊNCIA ANUAL DE HIV E AIDS PARA AS ESTIMATIVAS DE TODA A POPULAÇÃO BRASILEIRA 
# ==============================================================================
# Casos de aids identificados no Brasil
# Freqüência por Ano Notificação segundo Ano Notificação
# Período: 1980,1982-2024

# CASOS DE AIDS NOTIFICADOS NO SINAN, DECLARADOS NO SIM E REGISTRADOS NO SISCEL/SICLOM(1), SEGUNDO CAPITAL DE RESIDÊNCIA POR ANO DE DIAGNÓSTICO. BRASIL, 1980-2024 (2,3)

# Fonte: MS/SVSA/Departamento de HIV, Aids, Tuberculose, Hepatites Virais e Infecções Sexualmente Transmissíveis (Dathi).
# Notas: (1) Siclom utilizado para validação dos dados do Siscel. (2) Sinan e Siscel até 30/06/2024 e SIM de 2000 a 2023. (3) Dados preliminares sujeitos à alteração.

# ACESSE EM
# http://www2.aids.gov.br/cgi/tabcgi.exe?tabnet/br.def
# 1) cole aqui exatamente todo o texto que você colou (entre as aspas)
texto <- '
"Ano Notificação";"1980";"1982";"1983";"1984";"1985";"1986";"1987";"1988";"1989";"1990";"1991";"1992";"1993";"1994";"1995";"1996";"1997";"1998";"1999";"2000";"2001";"2002";"2003";"2004";"2005";"2006";"2007";"2008";"2009";"2010";"2011";"2012";"2013";"2014";"2015";"2016";"2017";"2018";"2019";"2020";"2021";"2022";"2023";"2024";"Total"
"1980";1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1
"1982";0;4;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;4
"1983";0;0;30;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;30
"1984";0;0;0;93;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;93
"1985";0;0;0;0;406;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;406
"1986";0;0;0;0;0;786;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;786
"1987";0;0;0;0;0;0;1897;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1897
"1988";0;0;0;0;0;0;0;3135;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;3135
"1989";0;0;0;0;0;0;0;0;4950;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;4950
"1990";0;0;0;0;0;0;0;0;0;6562;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;6562
"1991";0;0;0;0;0;0;0;0;0;0;9215;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;9215
"1992";0;0;0;0;0;0;0;0;0;0;0;12092;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;12092
"1993";0;0;0;0;0;0;0;0;0;0;0;0;14250;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;14250
"1994";0;0;0;0;0;0;0;0;0;0;0;0;0;15098;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;15098
"1995";0;0;0;0;0;0;0;0;0;0;0;0;0;0;16879;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;16879
"1996";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;20089;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;20089
"1997";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22208;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22208
"1998";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27185;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27185
"1999";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24814;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24814
"2000";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;23808;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;23808
"2001";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24245;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24245
"2002";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24320;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24320
"2003";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24087;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;24087
"2004";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;26334;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;26334
"2005";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;29431;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;29431
"2006";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27716;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27716
"2007";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22835;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22835
"2008";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27512;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27512
"2009";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;29864;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;29864
"2010";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;30591;0;0;0;0;0;0;0;0;0;0;0;0;0;0;30591
"2011";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;35795;0;0;0;0;0;0;0;0;0;0;0;0;0;35795
"2012";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;33949;0;0;0;0;0;0;0;0;0;0;0;0;33949
"2013";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;34311;0;0;0;0;0;0;0;0;0;0;0;34311
"2014";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;32727;0;0;0;0;0;0;0;0;0;0;32727
"2015";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;30941;0;0;0;0;0;0;0;0;0;30941
"2016";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;29808;0;0;0;0;0;0;0;29808
"2017";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;33222;0;0;0;0;0;0;33222
"2018";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;27254;0;0;0;0;0;0;27254
"2019";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;26711;0;0;0;0;0;26711
"2020";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;19429;0;0;0;0;19429
"2021";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22474;0;0;22474
"2022";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22784;0;22784
"2023";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22511;22511
"2024";0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;10694;10694
"Total";1;4;30;93;406;786;1897;3135;4950;6562;9215;12092;14250;15098;16879;20089;22208;27185;24814;23808;24245;24320;24087;26334;29431;27716;22835;27512;29864;30591;35795;33949;34311;32727;30941;29808;33222;27254;26711;19429;22474;22784;22511;10694;853047
'

# 2) leitura robusta via textConnection (respeita aspas e ; como separador)
con <- textConnection(texto)

aids <- read.table(con,
                 sep = ";",
                 dec = ",",
                 header = TRUE,
                 quote = "\"",
                 fill = TRUE,
                 stringsAsFactors = FALSE,
                 strip.white = TRUE,
                 comment.char = "") # desabilita comentários iniciados por #
close(con)


# =============================================================================
# MERGE DATAFRAMES
# =============================================================================

anos <- 1980:1991
pop_brasil_1980_1991 <- c(119000000, 122000000, 124200000, 126300000, 128400000, 130500000, 132500000, 134600000, 136700000, 138800000, 149000000, 146830000)

# Criar um data frame com as duas variáveis
pop_brasil_df <- tibble(
  ano = anos,
  populacao = pop_brasil_1980_1991
)

# Supondo que df_pop já existe com anos 1992 a 2025
# pop_brasil_df tem anos 1980 a 1991

# Unir os dois data frames em ordem cronológica
pop_total_df <- bind_rows(pop_brasil_df, df_pop) %>%
  arrange(ano)  # garante que os anos fiquem ordenados

# Visualizar o resultado
pop_total_df

# =============================================================================
# MERGE DATAFRAMES
# =============================================================================

# Seleciona as colunas de interesse e converte 'ano' para integer
aids_df <- aids %>%
  select(Ano.Notificação, Total) %>%
  rename(
    ano = Ano.Notificação,
    casos_totais = Total
  ) %>%
  mutate(ano = as.integer(ano))  # converte para inteiro

# Faz o merge com pop_total_df usando 'ano' como chave
pop_aids_df <- pop_total_df %>%
  left_join(aids_df, by = "ano")

# Visualizar o resultado
pop_aids_df


# =============================================================================
# REGRESSÃO LINEAR GENERALIZADA PAR TRATAR NAs
# =============================================================================

#  Preparar dados: separar observações com dados válidos
dados_modelo <- pop_aids_df %>%
  filter(!is.na(casos_totais))

view(dados_modelo)

pop_aids_df$casos_totais[23] <- NA
pop_aids_df$casos_totais_imput[23] <- NA

# Ajustar modelo GLM Poisson
# Incluindo 'populacao' como offset (opcional, para modelar taxa) e 'ano' como preditor
glm_modelo <- glm(casos_totais ~ ano + offset(log(populacao)),
                  family = poisson(link = "log"),
                  data = dados_modelo)


idx_na <- which(is.na(pop_aids_df$casos_totais))

pop_aids_df$casos_totais_imput <- pop_aids_df$casos_totais  # inicializa coluna

pop_aids_df$casos_totais_imput[idx_na] <- predict(glm_modelo, 
                                                  newdata = pop_aids_df[idx_na, ],
                                                  type = "response")  # valores preditos em escala de contagem

view(pop_aids_df)

pop_aids_df$casos_totais[2] <- 2.000
pop_aids_df$casos_totais_imput[2] <- 2.000
pop_aids_df$casos_totais_imput[24] <- 22771

View(pop_aids_df)

# ---------------------------------
# CALCULANDO A TAXA DE INSIDÊNCIA (coeficiente de indicência), por 100k habitantes

pop_aids_df <- pop_aids_df %>%
  mutate(
    coef_insiden = (casos_totais_imput / populacao) * 100000
  )

# view(pop_aids_df)


# =============================================================================
# MERGINDO A TAXA DE INSIDENCIA DO IBGE COM A MINHA
# =============================================================================
# Fonte: Ministério da Saúde/SVS/Programa Nacional de DST/Aids
# https://seriesestatisticas.ibge.gov.br/series.aspx?no=13&op=0&vcodigo=MS50&t=taxa-incidencia-aids

df_ibge <- read.csv("C:/Users/ryall/Downloads/taxa_incidencia_aids_1990-2008.csv", sep = ",")
  
ibge <- df_ibge %>%
  pivot_longer(
      cols = everything(),            
      names_to = "ano",
      values_to = "taxa_incidencia") %>%
  mutate(ano = as.integer(sub("X", "", ano)))

df_final <- pop_aids_df %>%
  left_join(
    ibge %>% 
      select(ano, taxa_incidencia) %>%  # pega só o que interessa
      rename(incidencia_ibge = taxa_incidencia),  # renomeia a coluna
    by = "ano"  # faz a linkagem pelo ano
  )

view(df_final)

# write.csv(df_final, "C:/Users/ryall/Desktop/R/biopolitica/biopolitcs/dataframes/df_aids.csv")

# =============================================================================
# PLOTTAGEM DAS SERIES HISTÓRICAS
# =============================================================================

df_long <- read.csv("C:/Users/ryall/Desktop/DDS/DADOS/assuntoparlamentar.csv")

df_long <- read.csv("https://raw.githubusercontent.com/ryallmeida/biopolitcs/refs/heads/main/dataframes/camaradata_long.csv")

df_final <- read.csv("https://raw.githubusercontent.com/ryallmeida/biopolitcs/refs/heads/main/dataframes/df_aids.csv")

contagem_prep <- read.csv("https://raw.githubusercontent.com/ryallmeida/biopolitcs/refs/heads/main/dataframes/contagem_prep.csv")

# -----------------------------------------------
# 1) Garantindo tipos numéricos corretos
# -----------------------------------------------

dplyr::glimpse(df_final)

df_long$ano   <- as.integer(df_long$ano)
df_final$ano  <- as.integer(df_final$ano)

df_final$coef_insiden    <- as.numeric(df_final$coef_insiden)

df_final$incidencia_ibge <- as.numeric(gsub(",", ".", df_final$incidencia_ibge))

df_final <- df_final[-41, ] 

# -----------------------------------------------
# 2) Definição do coeficiente de escala
# -----------------------------------------------

max_barras <- max(df_long$quantidade, na.rm = TRUE)

max_taxas_validas <- max(
  df_final$coef_insiden,
  df_final$incidencia_ibge,
  na.rm = TRUE
)

coef <- max_barras / max_taxas_validas


# -----------------------------------------------
# 3) Gráfico base (p1)
# -----------------------------------------------

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
  
  scale_x_continuous(
    breaks = seq(min(df_long$ano, na.rm = TRUE), 
                 max(df_long$ano, na.rm = TRUE), 1)
  ) +
  
  labs(
    x = "Ano",
    y = "Quantidade de proposições",
    fill = "Matéria das proposições parlamentares"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 90, 
                               vjust = 0.5, 
                               hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )


# -----------------------------------------------
# 4) Adicionando as linhas com eixo secundário
# -----------------------------------------------

p_final <- p1 +
  
  # Linha da minha estimativa (vermelha e pontilhada)
  geom_line(
    data = df_final,
    aes(x = ano, 
        y = coef_insiden * coef, 
        color = "Estimativa", 
        linetype = "Estimativa"),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  
  # Linha IBGE
  geom_line(
    data = df_final |> dplyr::filter(!is.na(incidencia_ibge)),
    aes(x = ano, 
        y = incidencia_ibge * coef, 
        color = "IBGE", 
        linetype = "IBGE"),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  
  # Definindo cores e tipos de linha na legenda
  scale_color_manual(
    name = "Coeficiêntes",
    values = c(
      "Estimativa" = "red",
      "IBGE" = "blue4"
    )
  ) +
  scale_linetype_manual(
    name = "Coeficiêntes",
    values = c(
      "Estimativa" = "dashed",
      "IBGE" = "solid"
    )
  ) +
  
  scale_y_continuous(
    name = "Quantidade de menções",
    sec.axis = sec_axis(
      ~ . / coef,
      name = "Coeficientes de Incidência"
    )
  ) +
  
  theme(
    axis.title.y.right = element_text(face = "bold"),
    legend.position = "top"
  )

# Exibir
#view(df_final)

print(p_final)

#ggsave("C:/Users/ryall/Downloads/parlamento&&aids.png", plot = p_final, width = 10, height = 6, dpi = 300)

# =============================================================================
# PLOTANDO USUÁRIOS DE PREP
# # =============================================================================

# Garantir ano como inteiro
contagem_prep$Ano <- as.integer(contagem_prep$Ano)

# Juntar com df_final pela coluna 'ano'
prep_join <- contagem_prep |> 
  dplyr::left_join(df_final, by = c("Ano" = "ano"))

# Criar coeficiente por 100 mil habitantes
prep_join <- prep_join |>
  dplyr::mutate(
    coef_prep = (Total_EmPrEP / populacao) * 100000
  )

# NA SÉRIE QUE EU EXTREI DO IBGE ESTAVA FALTANDO DADOS DA POPULAÇÃO RESIDENTE E ESTIMADA PARA OS ANOS DE 2022 E 2023
#  PROCUREI NA INTERNET E VOU IMPUTÁ-LOS DE FORMA MANUAL NO BANDO DE DADOS ACIMA

prep_join$populacao[5] <- 203062512 
prep_join$populacao[6] <- 212583750

# View(prep_join)
# deu certo 

# -----------------------------------------------
# 5) Adicionar a terceira série ao gráfico
# -----------------------------------------------

p3 <- p_final +
  
  # Linha 1 — Estimativa
  geom_line(
    data = df_final,
    aes(x = ano, 
        y = coef_insiden * coef, 
        color = "Estimativa", 
        linetype = "Estimativa"),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  
  # Linha 2 — IBGE
  geom_line(
    data = df_final |> filter(!is.na(incidencia_ibge)),
    aes(x = ano, 
        y = incidencia_ibge * coef, 
        color = "IBGE", 
        linetype = "IBGE"),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  
  # Linha 3 — PrEP coef por 100 mil (cinza contínua)
  geom_line(
    data = prep_join |> filter(!is.na(coef_prep)),
    aes(x = Ano,
        y = coef_prep * coef),
    color = "grey30",
    linewidth = 1.2,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  
  # Label ao lado da linha PrEP
  geom_text(
    data = prep_join |> filter(!is.na(coef_prep)) |> slice_tail(n = 1),
    aes(
      x = Ano + 0.25,
      y = coef_prep * coef,
      label = "Em PrEP"
    ),
    color = "grey30",
    hjust = 0,
    vjust = 0.5,
    size = 5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  # Legendas das duas primeiras linhas
  scale_color_manual(
    name = "Coeficientes",
    values = c(
      "Estimativa" = "red",
      "IBGE" = "blue4"
    )
  ) +
  scale_linetype_manual(
    name = "Coeficientes",
    values = c(
      "Estimativa" = "dashed",
      "IBGE" = "solid"
    )
  ) +
  
  scale_y_continuous(
    name = "Quantidade de menções",
    sec.axis = sec_axis(
      ~ . / coef,
      name = "Coeficientes de Incidência"
    )
  ) +
  
  theme(
    axis.title.y.right = element_text(face = "bold"),
    legend.position = "top"
  )

print(p3)

# ggsave("C:/Users/ryall/Downloads/parlamento&&aids2.png", plot = p3, width = 10, height = 6, dpi = 300)
