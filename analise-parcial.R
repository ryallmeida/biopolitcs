if(!require("pacman")){
  install.packages("pacman")
}

pacman::p_load(tidyverse, 
               viridis,
               lubridate, 
               quanteda,
               quanteda.textstats,
               quanteda.textplots)

# ==============================================================================
# CARREGAMENTO DE DADOS
# ==============================================================================

dados <- readr::read_csv(
  unz(
    "C:/Users/ryall/Downloads/Tabulação dos dados da pesquisa.csv.zip",
    "Tabulação dos dados da pesquisa.csv"
  ),
  show_col_types = FALSE
)

dados <- dados %>%
  slice(-1)

dplyr::glimpse(dados)

# ==============================================================================
# IDADE
# ==============================================================================

dados <- dados %>%
  mutate(
    data_nascimento = lubridate::ymd(nascimento),
    idade = time_length(
      interval(data_nascimento, today()),
      unit = "years"
    ) %>% floor()
  )

# ==============================================================================
# RAÇA-COR
# ==============================================================================
prop.table(table(dados$cor_raça))

#    Branca  Indigena     Parda     Preta 
# 0.2857143 0.1428571 0.4285714 0.1428571 

# ==============================================================================
# RELIGIOSIDADE
# ==============================================================================
prop.table(table(dados$religiao))

#   catolica de_matriz_africana   outras_religioes 
#  0.1428571          0.2857143          0.1428571 

# sem_religiao 
#    0.4285714 

# ==============================================================================
# NIVEL DE INSTRUÇÃO FORMAL
# ==============================================================================
prop.table(table(dados$ensino))

dados$ensino[1] <- "medio_completo"

# fundamental_incompleto         medio_completo 
#              0.1428571              0.7142857   

#      superior_completo 
#              0.1428571 

# ==============================================================================
# TRABALHO
# ==============================================================================
prop.table(table(dados$trabalho))
# empregado_com_clt    nao_trabalha_atualmente 
#         0.4285714                  0.4285714 

# trabalha_por_conta_propria 
#                  0.1428571 

# ==============================================================================
# QUANTAS PESSOAS MORAM NO SEU DOMICILIO?
# ==============================================================================
sd(dados$domicilio_residencia)
# [1] 1.397276

summary(dados$domicilio_residencia)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   3.000   3.000   3.571   4.000   6.000

# ==============================================================================
# RENDA MENSAL FAMILIAR
# ==============================================================================

# AMOSTRA A SER REMOVIDA, DA PESSOA QUE NÃO SABE
# 1/7
# [1] CORRESPONDENTE A, 0.1428571

dados[1, 9] <- NA 

dados$renda_mensal <- as.double(dados$renda_mensal)

sd(dados$renda_mensal, na.rm = TRUE)
# [1] 1159.17

summary(dados$renda_mensal)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2000    2089    2656    2973    3384    5000       1 

# ==============================================================================
# MODOS DE SER E SE EXPRESSAR
# ==============================================================================

prop.table(table(dados$modo_de_ser))
# efeminado masculinizado      nao_sabe 
##0.4285714     0.4285714     0.1428571 

# ==============================================================================
# MODOS COMO A REDE DE CONTATOS SE CHAMA
# ==============================================================================

toks <- corpus(na.omit(dados$nomeacao_rede_contatos)) |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("pt")) |>
  tokens_ngrams(n = 1:2)

dfm_rede <- dfm(toks)

# Criar matriz documento-termo
dfm_rede <- dfm(toks)

freq <- quanteda.textstats::textstat_frequency(dfm_rede)


# Frequência das 100 palavras mais comuns
tstat_freq_rede <- textstat_frequency(dfm_rede, n = 100)

# Plot
ggplot(tstat_freq_rede,
       aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  labs(x = "Frequência", y = "N-grama") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14)
  )


s# ==============================================================================
# POSICIONAMENTO SEXUAL
# ==============================================================================

prop.table(table(dados$praticas_sexuais))

#   exclusivamente_ativo exclusivamente_passivo 
#              0.2857143              0.1428571 

#               versatil         versatil_ativo 
#              0.2857143              0.1428571 

#       versatil_passivo 
#              0.1428571 

# ==============================================================================
# RELACIONAMENTO E SUAS NATUREZA
# ==============================================================================

  3/7
[1] 0.4285714
