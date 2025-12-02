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

pacman::p_load(httr2, 
               jsonlite, 
               tidyverse)

# =============================================================================


# Dependências
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)

# Base da API
base_api <- "https://legis.senado.leg.br/dadosabertos"

# Helper para obter e parsear JSON (ou retornar NULL e logar)

# Função fetch_json corrigida
fetch_json <- function(url, verbose = TRUE) {
  Sys.sleep(0.2) # pequeno delay para ser gentil com o servidor
  res <- tryCatch({
    r <- httr::GET(url, httr::user_agent("R (consulta para pesquisa acadêmica)"))
    status <- httr::status_code(r)
    if (status >= 200 && status < 300) {
      text <- httr::content(r, as = "text", encoding = "UTF-8")
      if (nchar(text) == 0) return(NULL)
      parsed <- tryCatch(
        jsonlite::fromJSON(text, simplifyVector = FALSE),
        error = function(e) {
          # Se não for JSON, devolve o texto cru e marca o parse error com nome válido
          return(list(raw = text, "_parse_error" = TRUE))
        }
      )
      return(parsed)
    } else {
      if (verbose) message(sprintf("HTTP %s => %s", status, url))
      return(NULL)
    }
  }, error = function(e) {
    if (verbose) message("Erro fetch: ", conditionMessage(e), " | url: ", url)
    return(NULL)
  })
  return(res)
}

# Função que tenta diversos padrões para buscar lista de matérias por ano
fetch_materias_por_ano <- function(ano) {
  patterns <- c(
    sprintf("%s/materia/lista?ano=%s&formato=json", base_api, ano),
    sprintf("%s/materia?ano=%s&formato=json", base_api, ano),
    sprintf("%s/materia/porAno/%s.json", base_api, ano),
    sprintf("%s/materia/lista/%s.json", base_api, ano),
    sprintf("%s/materia/lista?anoApresentacao=%s&formato=json", base_api, ano)
  )
  for (url in patterns) {
    got <- fetch_json(url)
    if (!is.null(got)) {
      # heurística: se o resultado contém "Materias" ou "materia" ou "ListaMateria"
      keys <- names(got)
      if (length(keys) > 0 && any(grepl("materi|Materia|Materias|lista", keys, ignore.case = TRUE))) {
        return(list(data = got, url = url))
      }
      # se o retorno for uma lista de objetos (array), assumir que é a lista
      if (is.list(got) && length(got) > 0 && is.list(got[[1]])) {
        return(list(data = got, url = url))
      }
    }
  }
  # se chegou aqui, nenhuma pattern funcionou
  message(sprintf("Nenhum endpoint padrão retornou matérias para o ano %s (tente verificar o Swagger).", ano))
  return(NULL)
}

# Função para extrair/normalizar campos básicos de uma matéria JSON (heurística)
normalize_materia <- function(m) {
  # m pode ter estrutura variada; usamos heurísticas para extrair campos importantes
  cod <- m$codMateria %||% m$codigoMateria %||% m$idMateria %||% m$codMateriaLegislativo %||% NA
  sigla <- m$siglaTipoMateria %||% m$siglaTipo %||% m$sigla %||% NA
  numero <- m$numeroMateria %||% m$numero %||% NA
  ano <- m$anoApresentacao %||% m$ano %||% NA
  ementa <- m$ementa %||% m$emento %||% m$ementaMateria %||% NA
  data_apresent <- m$dataApresentacao %||% m$dataApresentacaoFormatada %||% m$dataApresentacaoMateria %||% NA
  situacao <- m$situacaoAtual %||% m$situacao %||% NA
  autores <- NA
  # alguns endpoints trazem autores como lista
  if (!is.null(m$autores) && length(m$autores)>0) {
    autores <- paste(unlist(lapply(m$autores, function(a) a$nomeParlamentar %||% a$nome)), collapse = "; ")
  } else {
    # fallback para campo autor
    autores <- m$autor %||% m$autorTexto %||% NA
  }
  tibble(
    codMateria = as.character(cod),
    sigla = as.character(sigla),
    numero = as.character(numero),
    ano = as.integer(ano),
    ementa = as.character(ementa),
    data_apresentacao = as.character(data_apresent),
    autores = as.character(autores),
    situacao_atual = as.character(situacao)
  )
}

# operador null coalescing simples
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Principais endpoints por matéria (tentativas)
get_relatorias_for_cod <- function(cod) {
  urls <- c(
    sprintf("%s/materia/relatorias/%s.json", base_api, cod),
    sprintf("%s/materia/relatorias/%s", base_api, cod),
    sprintf("%s/relatorias/materia/%s.json", base_api, cod)
  )
  for (u in urls) {
    v <- fetch_json(u, verbose = FALSE)
    if (!is.null(v)) return(v)
  }
  return(NULL)
}

get_tramitacoes_for_cod <- function(cod) {
  urls <- c(
    sprintf("%s/materia/movimentos/%s.json", base_api, cod),
    sprintf("%s/materia/tramitacoes/%s.json", base_api, cod),
    sprintf("%s/materia/tramitacao/%s.json", base_api, cod)
  )
  for (u in urls) {
    v <- fetch_json(u, verbose = FALSE)
    if (!is.null(v)) return(v)
  }
  return(NULL)
}

get_autoria_for_cod <- function(cod) {
  urls <- c(
    sprintf("%s/materia/distribuicao/autoria/%s.json", base_api, cod),
    sprintf("%s/materia/autoria/%s.json", base_api, cod),
    sprintf("%s/materia/autor/%s.json", base_api, cod)
  )
  for (u in urls) {
    v <- fetch_json(u, verbose = FALSE)
    if (!is.null(v)) return(v)
  }
  return(NULL)
}

# Loop por anos e coleta
anos <- 1980:2024
todos_logs <- list()
result_list <- list()
i_row <- 1

for (ano in anos) {
  message(">> ano: ", ano)
  res_ano <- fetch_materias_por_ano(ano)
  if (is.null(res_ano)) {
    todos_logs[[as.character(ano)]] <- list(ok = FALSE, motivo = "no_endpoint_found")
    next
  }
  payload <- res_ano$data
  url_used <- res_ano$url
  message("  endpoint usado: ", url_used)
  # payload pode ter várias formas. Tentar extrair uma lista de matérias
  materias_raw <- NULL
  if (!is.null(payload$Materias)) materias_raw <- payload$Materias
  if (is.null(materias_raw) && !is.null(payload$materia)) materias_raw <- payload$materia
  if (is.null(materias_raw) && is.list(payload) && length(payload)>0 && is.list(payload[[1]])) materias_raw <- payload
  if (is.null(materias_raw)) {
    # fallback: se payload tiver um campo qualquer com lista única
    materias_raw <- payload
  }
  # garantir que é vetor/lista de matérias
  if (is.null(materias_raw) || length(materias_raw)==0) {
    todos_logs[[as.character(ano)]] <- list(ok = FALSE, motivo = "empty_year_result", url = url_used)
    next
  }
  todos_logs[[as.character(ano)]] <- list(ok = TRUE, n = length(materias_raw), url = url_used)
  # iterar matérias
  for (m in materias_raw) {
    norm <- tryCatch(normalize_materia(m), error = function(e) tibble(
      codMateria = NA, sigla = NA, numero = NA, ano = ano,
      ementa = NA, data_apresentacao = NA, autores = NA, situacao_atual = NA
    ))
    cod <- as.character(norm$codMateria[1])
    # buscar relatorias/tramitacoes/autoria (se tivermos cod)
    rels <- if (!is.na(cod) && nzchar(cod)) get_relatorias_for_cod(cod) else NULL
    trams <- if (!is.na(cod) && nzchar(cod)) get_tramitacoes_for_cod(cod) else NULL
    auts <- if (!is.na(cod) && nzchar(cod)) get_autoria_for_cod(cod) else NULL
    # montar linha final
    row <- norm %>%
      mutate(
        relatorias_json = if (!is.null(rels)) jsonlite::toJSON(rels, auto_unbox = TRUE) else NA_character_,
        tramitacoes_json = if (!is.null(trams)) jsonlite::toJSON(trams, auto_unbox = TRUE) else NA_character_,
        autoria_json = if (!is.null(auts)) jsonlite::toJSON(auts, auto_unbox = TRUE) else NA_character_,
        fonte_cod_endpoint = url_used
      )
    result_list[[i_row]] <- row
    i_row <- i_row + 1
  }
}

# combinar tudo num data.frame
if (length(result_list) == 0) {
  stop("Nenhuma matéria coletada — verifique endpoints e o Swagger do Senado.")
}
df <- bind_rows(result_list)

# Ajustes finais: converter datas quando possível
df <- df %>% mutate(
  data_apresentacao = dplyr::na_if(data_apresentacao, "NA"),
  data_apresentacao = ifelse(!is.na(data_apresentacao) & nzchar(data_apresentacao),
                             as.character(data_apresentacao), NA_character_)
)

# salvar
readr::write_csv(df, "materias_1980_2024.csv")
saveRDS(df, "materias_1980_2024.rds")
message("Salvo: materias_1980_2024.csv  e materias_1980_2024.rds")
message("Total de matérias coletadas: ", nrow(df))

# Relatório simples de logs
logdf <- tibble(
  ano = names(todos_logs),
  ok = sapply(todos_logs, function(x) ifelse(is.null(x$ok), FALSE, x$ok)),
  detalhe = sapply(todos_logs, function(x) paste0(names(x), ":", paste(unlist(x), collapse = "|")))
)
print(logdf)
