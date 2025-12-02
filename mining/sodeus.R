# carregue libs necessárias
if (!requireNamespace("httr", quietly=TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly=TRUE)) install.packages("jsonlite")
if (!requireNamespace("xml2", quietly=TRUE)) install.packages("xml2")
library(httr); library(jsonlite); library(xml2)

base_api <- "https://legis.senado.leg.br/dadosabertos"

# endpoints candidatos para lista por ano
anos_test <- c(2023, 2010)
endpoints_por_ano <- c(
  sprintf("%s/materia/lista?ano=%s&formato=json", base_api, "%s"),
  sprintf("%s/materia?ano=%s&formato=json", base_api, "%s"),
  sprintf("%s/materia/porAno/%s.json", base_api, "%s"),
  sprintf("%s/materia/lista/%s.json", base_api, "%s"),
  sprintf("%s/materia/lista?anoApresentacao=%s&formato=json", base_api, "%s"),
  sprintf("%s/materia?anoApresentacao=%s", base_api, "%s"),
  sprintf("%s/materia?ano=%s", base_api, "%s")
)

# endpoints candidatos para relatorias/tramitacoes por codMateria
cod_exemplo <- "149"  # troque se quiser
endpoints_por_cod <- c(
  sprintf("%s/materia/relatorias/%s.json", base_api, cod_exemplo),
  sprintf("%s/materia/relatorias/%s", base_api, cod_exemplo),
  sprintf("%s/materia/relatorias/%s.xml", base_api, cod_exemplo),
  sprintf("%s/materia/movimentos/%s.json", base_api, cod_exemplo),
  sprintf("%s/materia/tramitacoes/%s.json", base_api, cod_exemplo),
  sprintf("%s/materia/%s?formato=json", base_api, cod_exemplo),
  sprintf("%s/materia/%s", base_api, cod_exemplo)
)

# função teste simples que mostra status, content-type e início do corpo
test_url <- function(url) {
  cat("\n### Testando:", url, "\n")
  Sys.sleep(0.3)
  res <- tryCatch(httr::GET(url, httr::user_agent("R (diagnóstico)")), error = function(e) { return(e) })
  if (inherits(res, "error")) {
    cat("Erro requisição:", conditionMessage(res), "\n")
    return(invisible(NULL))
  }
  cat("HTTP status:", httr::status_code(res), "\n")
  ct <- headers(res)[["content-type"]]
  cat("Content-Type:", ct, "\n")
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (nchar(txt) == 0) {
    cat("Resposta vazia\n")
    return(invisible(NULL))
  }
  cat("Primeiros 800 caracteres da resposta:\n")
  cat(substr(txt, 1, 800), "\n\n")
  # tenta identificar JSON vs XML
  if (grepl("^\\s*\\{", txt) || grepl("^\\s*\\[", txt)) {
    cat("=> Parece JSON. Tentando parse...\n")
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) e)
    if (inherits(parsed, "error")) cat("Parse JSON falhou:", conditionMessage(parsed), "\n") else cat("Parse JSON OK — tipo:", class(parsed), "\n")
  } else if (grepl("^\\s*<\\?xml", txt) || grepl("^\\s*<", txt)) {
    cat("=> Parece XML. Tentando parse com xml2...\n")
    parsed_xml <- tryCatch(xml2::read_xml(txt), error = function(e) e)
    if (inherits(parsed_xml, "error")) cat("Parse XML falhou:", conditionMessage(parsed_xml), "\n") else cat("Parse XML OK — nó raiz:", xml_name(parsed_xml), "\n")
  } else {
    cat("=> Texto livre (não JSON nem XML detectado)\n")
  }
  return(invisible(NULL))
}

# roda os testes por ano (apenas alguns exemplos)
for (ano in anos_test) {
  cat("\n#### TESTE ANO:", ano, "####\n")
  for (pat in endpoints_por_ano) {
    url <- sprintf(pat, ano)
    test_url(url)
  }
}

# testa alguns endpoints por codMateria
cat("\n==== TESTE POR codMateria ====\n")
for (u in endpoints_por_cod) test_url(u)

# require: httr, jsonlite, xml2
`%||%` <- function(a, b) if (!is.null(a)) a else b

fetch_json_or_xml <- function(url, verbose=TRUE, sleep = 0.25) {
  Sys.sleep(sleep)
  res <- tryCatch(httr::GET(url, httr::user_agent("R (consulta acadêmica)")), error = function(e) e)
  if (inherits(res, "error")) {
    if (verbose) message("Erro HTTP: ", conditionMessage(res), " | url: ", url)
    return(list(ok = FALSE, status = NA, url = url, error = conditionMessage(res)))
  }
  status <- httr::status_code(res)
  ct <- headers(res)[["content-type"]] %||% ""
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (status < 200 || status >= 300) {
    if (verbose) message(sprintf("HTTP %s para %s (Content-Type: %s)", status, url, ct))
    return(list(ok = FALSE, status = status, url = url, content_type = ct, raw = txt))
  }
  if (nchar(txt) == 0) return(list(ok = TRUE, status = status, url = url, content_type = ct, parsed = NULL, raw = txt))
  # tenta JSON
  parsed_json <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) e)
  if (!inherits(parsed_json, "error")) {
    return(list(ok = TRUE, status = status, url = url, content_type = ct, parsed = parsed_json, raw = txt, format = "json"))
  }
  # tenta XML
  parsed_xml <- tryCatch(xml2::read_xml(txt), error = function(e) e)
  if (!inherits(parsed_xml, "error")) {
    return(list(ok = TRUE, status = status, url = url, content_type = ct, parsed = parsed_xml, raw = txt, format = "xml"))
  }
  # fallback: não parseável
  return(list(ok = TRUE, status = status, url = url, content_type = ct, parsed = NULL, raw = txt, format = "text", parse_error = TRUE))
}


library(httr)
library(jsonlite)

url <- "https://legis.senado.leg.br/dadosabertos/materia/130065.json"
x <- GET(url)
status_code(x)
txt <- content(x, as = "text", encoding = "UTF-8")
substr(txt, 1, 300)

url <- "https://legis.senado.leg.br/dadosabertos/senador/lista/atual.json"
x <- fromJSON(url)
length(x$ListaParlamentarEmExercicio$Parlamentares$Parlamentar)

fromJSON("https://legis.senado.leg.br/dadosabertos/materia/tramitacao/130065.json")

fromJSON("https://legis.senado.leg.br/dadosabertos/materia/141694.json")


# ==========================================================
# SCRIPT ÚNICO E COMPLETO – COLETAR PROCESSOS DO SENADO (API NOVA)
# ==========================================================

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# ----------------------------------------------------------
# Função segura para baixar e parsear JSON da API nova
# ----------------------------------------------------------
baixar_processo <- function(id) {
  url <- paste0("https://legis.senado.leg.br/dadosabertos/processo/", id)
  
  resp <- try(GET(url), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (status_code(resp) != 200) return(NULL)
  
  txt <- content(resp, as = "text", encoding = "UTF-8")
  
  # Se não for JSON válido, descartar
  json <- try(fromJSON(txt), silent = TRUE)
  if (inherits(json, "try-error")) return(NULL)
  if (!"Processo" %in% names(json)) return(NULL)
  
  p <- json$Processo
  
  # Criar tibble padronizado
  tibble(
    idProcesso     = id,
    idMateria      = p$IdentificacaoMateria$CodigoMateria %||% NA,
    siglaMateria   = p$IdentificacaoMateria$SiglaSubtipoMateria %||% NA,
    tipoMateria    = p$IdentificacaoMateria$DescricaoSubtipoMateria %||% NA,
    numeroMateria  = p$IdentificacaoMateria$NumeroMateria %||% NA,
    anoMateria     = p$IdentificacaoMateria$AnoMateria %||% NA,
    descricao      = p$IdentificacaoMateria$DescricaoIdentificacaoMateria %||% NA,
    dataApresentacao = p$DadosBasicosMateria$DataApresentacao %||% NA,
    dataLeitura      = p$DadosBasicosMateria$DataLeitura %||% NA,
    ementa           = p$DadosBasicosMateria$EmentaMateria %||% NA,
    autor            = p$DadosBasicosMateria$Autor %||% NA,
    natureza         = p$DadosBasicosMateria$NaturezaMateria$NomeNatureza %||% NA
  )
}

# Operador seguro
`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------------------------------------
# INTERVALO DE PROCESSOS A BAIXAR
# ----------------------------------------------------------
# A numeração real dos processos está entre 5 milhões e 9 milhões (aprox.)
# Você pode ajustar depois!
inicio <- 50000
fim    <- 50000 # coleta 50 mil processos (ajuste como quiser)

ids <- seq(inicio, fim)

message("Iniciando coleta total de: ", length(ids), " processos...")

# ----------------------------------------------------------
# COLETA EM LOOP SEGURO
# ----------------------------------------------------------
resultados <- map_dfr(ids, function(i) {
  out <- baixar_processo(i)
  if (!is.null(out)) {
    message("✔ Processo encontrado: ", i)
    return(out)
  } else {
    return(NULL)
  }
})

# ----------------------------------------------------------
# FILTRAR POR ANO (1980–2024)
# ----------------------------------------------------------
dados_final <- resultados %>%
  mutate(
    anoMateria = suppressWarnings(as.numeric(anoMateria))
  ) %>%
  filter(!is.na(anoMateria),
         anoMateria >= 1980,
         anoMateria <= 2024)

# ----------------------------------------------------------
# SALVAR RESULTADO
# ----------------------------------------------------------
saveRDS(dados_final, "processos_senado_1980_2024.rds")
write.csv(dados_final, "processos_senado_1980_2024.csv", row.names = FALSE)

message("✔ Concluído!")
message("Total de processos obtidos: ", nrow(dados_final))
message("Arquivos salvos: processos_senado_1980_2024.rds e .csv")

