# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# LABORATÓRIO DE ESTUDO DA SEXUALIDADE HUMANA (LABESHU)
# DEPARTAMENTO DE PSICOLOGIA

# --------------------------------
# CODADO ORIGINALMENTE EM R, VERSÃO 4.5.2
# --------------------------------
# DE AUTORIA DE RYAN ALMEIDA

# TRATAMENTO NOS MICRODADOS DO PAINEL PREP, DISPONÍVEL EM:
# https://www.gov.br/aids/pt-br/indicadores-epidemiologicos/painel-de-monitoramento/painel-prep

dados <- read.csv("C:/Users/ryall/Downloads/Dados_PrEP_transparencia/Banco_PrEP_usuarios.csv")

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
