#' SIDRA Series Metadata for PNADC Mensalization
#'
#' This file contains metadata definitions for all 86 SIDRA series used in the
#' mensalization process. It maps series names to their SIDRA API endpoints,
#' table IDs, variable codes, and categorization.
#'
#' @name sidra-series-metadata
#' @keywords internal
NULL

#' Get SIDRA Series Metadata
#'
#' Returns a data.table with metadata for all PNADC rolling quarter series
#' available from IBGE's SIDRA API.
#'
#' @param series Character vector of series names to retrieve, or "all" (default)
#'   for all series.
#' @param category Character vector of categories to filter by. Valid categories:
#'   "rate", "population", "employment", "sector", "income_nominal", "income_real",
#'   "underutilization", "price_index". Use NULL (default) for no filtering.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{series_name}{Character. Internal name used in the package}
#'     \item{api_path}{Character. SIDRA API path for get_sidra()}
#'     \item{table_id}{Integer. SIDRA table number}
#'     \item{variable_id}{Integer. SIDRA variable code}
#'     \item{category}{Character. Series category}
#'     \item{description_pt}{Character. Portuguese description}
#'     \item{unit}{Character. Unit of measurement ("thousands", "rate", "currency")}
#'     \item{is_derived}{Logical. TRUE if computed from other series}
#'     \item{requires_deflation}{Logical. TRUE if needs IPCA deflation}
#'   }
#'
#' @examples
#' \dontrun{
#' # Get all series metadata
#' meta <- get_sidra_series_metadata()
#'
#' # Get only population series
#' pop_meta <- get_sidra_series_metadata(category = "population")
#'
#' # Get specific series
#' meta <- get_sidra_series_metadata(series = c("taxadesocup", "popocup"))
#' }
#'
#' @export
get_sidra_series_metadata <- function(series = "all", category = NULL) {

  # Build complete metadata table

meta <- data.table::data.table(
    series_name = character(),
    api_path = character(),
    table_id = integer(),
    variable_id = integer(),
    classification_id = character(),
    classification_value = character(),
    category = character(),
    description_pt = character(),
    unit = character(),
    is_derived = logical(),
    requires_deflation = logical()
  )

  # ============================================================================
  # CATEGORY: RATES (from specific rate tables)
  # ============================================================================

  rates <- data.table::data.table(
    series_name = c("taxapartic", "nivelocup", "niveldesocup", "taxadesocup"),
    api_path = c(
      "/t/5944/n1/all/v/4096/p/all/d/v4096%201",
      "/t/6379/n1/all/v/4097/p/all/d/v4097%201",
      "/t/6380/n1/all/v/4098/p/all/d/v4098%201",
      "/t/6381/n1/all/v/4099/p/all/d/v4099%201"
    ),
    table_id = c(5944L, 6379L, 6380L, 6381L),
    variable_id = c(4096L, 4097L, 4098L, 4099L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    category = "rate",
    description_pt = c(
      "Taxa de participacao na forca de trabalho",
      "Nivel de ocupacao",
      "Nivel de desocupacao",
      "Taxa de desocupacao"
    ),
    unit = "rate",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: POPULATION
  # ============================================================================

  population <- data.table::data.table(
    series_name = c("populacao", "pop14mais", "popnaforca", "popocup", "popdesocup", "popforadaforca"),
    api_path = c(
      "/t/6022/n1/all/v/606/p/all",
      "/t/6318/n1/all/v/1641/p/all/c629/32385",
      "/t/6318/n1/all/v/1641/p/all/c629/32386",
      "/t/6318/n1/all/v/1641/p/all/c629/32387",
      "/t/6318/n1/all/v/1641/p/all/c629/32446",
      "/t/6318/n1/all/v/1641/p/all/c629/32447"
    ),
    table_id = c(6022L, 6318L, 6318L, 6318L, 6318L, 6318L),
    variable_id = c(606L, 1641L, 1641L, 1641L, 1641L, 1641L),
    classification_id = c(NA_character_, "c629", "c629", "c629", "c629", "c629"),
    classification_value = c(NA_character_, "32385", "32386", "32387", "32446", "32447"),
    category = "population",
    description_pt = c(
      "Populacao total residente",
      "Populacao de 14 anos ou mais de idade",
      "Populacao na forca de trabalho",
      "Populacao ocupada",
      "Populacao desocupada",
      "Populacao fora da forca de trabalho"
    ),
    unit = "thousands",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: EMPLOYMENT (by type/position)
  # ============================================================================

  employment <- data.table::data.table(
    series_name = c(
      "empregado", "empregpriv", "empregprivcomcart", "empregprivsemcart",
      "domestico", "domesticocomcart", "domesticosemcart",
      "empregpubl", "empregpublcomcart", "empregpublsemcart", "estatutmilitar",
      "empregador", "empregadorcomcnpj", "empregadorsemcnpj",
      "contapropria", "contapropriacomcnpj", "contapropriasemcnpj",
      "trabfamauxiliar"
    ),
    api_path = c(
      "/t/6320/n1/all/v/4090/p/all/c11913/96166",
      "/t/6320/n1/all/v/4090/p/all/c11913/31721",
      "/t/6320/n1/all/v/4090/p/all/c11913/31722",
      "/t/6320/n1/all/v/4090/p/all/c11913/31723",
      "/t/6320/n1/all/v/4090/p/all/c11913/31724",
      "/t/6320/n1/all/v/4090/p/all/c11913/31725",
      "/t/6320/n1/all/v/4090/p/all/c11913/31726",
      "/t/6320/n1/all/v/4090/p/all/c11913/31727",
      "/t/6320/n1/all/v/4090/p/all/c11913/31728",
      "/t/6320/n1/all/v/4090/p/all/c11913/31729",
      "/t/6320/n1/all/v/4090/p/all/c11913/31730",
      "/t/6320/n1/all/v/4090/p/all/c11913/96170",
      "/t/6320/n1/all/v/4090/p/all/c11913/45934",
      "/t/6320/n1/all/v/4090/p/all/c11913/45935",
      "/t/6320/n1/all/v/4090/p/all/c11913/96171",
      "/t/6320/n1/all/v/4090/p/all/c11913/45936",
      "/t/6320/n1/all/v/4090/p/all/c11913/45937",
      "/t/6320/n1/all/v/4090/p/all/c11913/31731"
    ),
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = c(
      "96166", "31721", "31722", "31723",
      "31724", "31725", "31726",
      "31727", "31728", "31729", "31730",
      "96170", "45934", "45935",
      "96171", "45936", "45937",
      "31731"
    ),
    category = "employment",
    description_pt = c(
      "Empregado (total)",
      "Empregado no setor privado",
      "Empregado no setor privado com carteira",
      "Empregado no setor privado sem carteira",
      "Trabalhador domestico",
      "Trabalhador domestico com carteira",
      "Trabalhador domestico sem carteira",
      "Empregado no setor publico",
      "Empregado no setor publico com carteira",
      "Empregado no setor publico sem carteira",
      "Militar e servidor estatutario",
      "Empregador (total)",
      "Empregador com CNPJ",
      "Empregador sem CNPJ",
      "Conta propria (total)",
      "Conta propria com CNPJ",
      "Conta propria sem CNPJ",
      "Trabalhador familiar auxiliar"
    ),
    unit = "thousands",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: SECTORS (economic activity groups)
  # Note: Using c888 classification (newer grouping), not c693 (older)
  # ============================================================================

  sectors <- data.table::data.table(
    series_name = c(
      "agropecuaria", "industria", "construcao", "comercio", "transporte",
      "alojaliment", "infcomfinimobadm", "adminpublica", "outroservico", "servicodomestico"
    ),
    api_path = c(
      "/t/6323/n1/all/v/4090/p/all/c888/47947",
      "/t/6323/n1/all/v/4090/p/all/c888/47948",
      "/t/6323/n1/all/v/4090/p/all/c888/47949",
      "/t/6323/n1/all/v/4090/p/all/c888/47950",
      "/t/6323/n1/all/v/4090/p/all/c888/56622",
      "/t/6323/n1/all/v/4090/p/all/c888/56623",
      "/t/6323/n1/all/v/4090/p/all/c888/56624",
      "/t/6323/n1/all/v/4090/p/all/c888/60032",
      "/t/6323/n1/all/v/4090/p/all/c888/56627",
      "/t/6323/n1/all/v/4090/p/all/c888/56628"
    ),
    table_id = 6323L,
    variable_id = 4090L,
    classification_id = "c888",
    classification_value = c(
      "47947", "47948", "47949", "47950", "56622",
      "56623", "56624", "60032", "56627", "56628"
    ),
    category = "sector",
    description_pt = c(
      "Ocupados na agropecuaria",
      "Ocupados na industria",
      "Ocupados na construcao",
      "Ocupados no comercio",
      "Ocupados em transporte, armazenagem e correio",
      "Ocupados em alojamento e alimentacao",
      "Ocupados em informacao, comunicacao, financeiras, imobiliarias e administrativas",
      "Ocupados na administracao publica, defesa, seguridade, educacao, saude e servicos sociais",
      "Ocupados em outros servicos",
      "Ocupados em servicos domesticos"
    ),
    unit = "thousands",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: INCOME - NOMINAL
  # ============================================================================

  income_nominal <- data.table::data.table(
    series_name = c("rendhabnominaltodos", "rendefetnominaltodos",
                    "massahabnominaltodos", "massaefetnominaltodos"),
    api_path = c(
      "/t/6390/n1/all/v/5929/p/all",
      "/t/6387/n1/all/v/5931/p/all",
      "/t/6392/n1/all/v/6288/p/all",
      "/t/6393/n1/all/v/6291/p/all"
    ),
    table_id = c(6390L, 6387L, 6392L, 6393L),
    variable_id = c(5929L, 5931L, 6288L, 6291L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    category = "income_nominal",
    description_pt = c(
      "Rendimento habitual nominal medio de todos os trabalhos",
      "Rendimento efetivo nominal medio de todos os trabalhos",
      "Massa de rendimento habitual nominal de todos os trabalhos",
      "Massa de rendimento efetivo nominal de todos os trabalhos"
    ),
    unit = c("currency", "currency", "currency_millions", "currency_millions"),
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: INCOME - REAL (requires IPCA deflation)
  # ============================================================================

  income_real <- data.table::data.table(
    series_name = c(
      "rendhabrealtodos", "rendefetrealtodos", "rendhabrealprinc", "rendefetrealprinc",
      "rhrpempregado", "rhrpempregpriv", "rhrpempregprivcomcart", "rhrpempregprivsemcart",
      "rhrpdomestico", "rhrpdomesticocomcart", "rhrpdomesticosemcart",
      "rhrpempregpubl", "rhrpempregpublcomcart", "rhrpempregpublsemcart", "rhrpestatutmilitar",
      "rhrpempregador", "rhrpempregadorcomcnpj", "rhrpempregadorsemcnpj",
      "rhrpcontapropria", "rhrpcontapropriacomcnpj", "rhrpcontapropriasemcnpj",
      "rhrpagropecuaria", "rhrpindustria", "rhrpconstrucao", "rhrpcomercio", "rhrptransporte",
      "rhrpalojaliment", "rhrpinfcomfinimobadm", "rhrpadminpublica", "rhrpoutroservico", "rhrpservicodomestico",
      "massahabrealtodos", "massaefetrealtodos"
    ),
    api_path = c(
      "/t/6390/n1/all/v/5933/p/all",
      "/t/6387/n1/all/v/5935/p/all",
      "/t/6389/n1/all/v/5932/p/all/c11913/96165",
      "/t/6388/n1/all/v/5934/p/all",
      "/t/6389/n1/all/v/5932/p/all/c11913/96166",
      "/t/6389/n1/all/v/5932/p/all/c11913/31721",
      "/t/6389/n1/all/v/5932/p/all/c11913/31722",
      "/t/6389/n1/all/v/5932/p/all/c11913/31723",
      "/t/6389/n1/all/v/5932/p/all/c11913/31724",
      "/t/6389/n1/all/v/5932/p/all/c11913/31725",
      "/t/6389/n1/all/v/5932/p/all/c11913/31726",
      "/t/6389/n1/all/v/5932/p/all/c11913/31727",
      "/t/6389/n1/all/v/5932/p/all/c11913/31728",
      "/t/6389/n1/all/v/5932/p/all/c11913/31729",
      "/t/6389/n1/all/v/5932/p/all/c11913/31730",
      "/t/6389/n1/all/v/5932/p/all/c11913/96170",
      "/t/6389/n1/all/v/5932/p/all/c11913/45934",
      "/t/6389/n1/all/v/5932/p/all/c11913/45935",
      "/t/6389/n1/all/v/5932/p/all/c11913/96171",
      "/t/6389/n1/all/v/5932/p/all/c11913/45936",
      "/t/6389/n1/all/v/5932/p/all/c11913/45937",
      "/t/6391/n1/all/v/5932/p/all/c888/47947",
      "/t/6391/n1/all/v/5932/p/all/c888/47948",
      "/t/6391/n1/all/v/5932/p/all/c888/47949",
      "/t/6391/n1/all/v/5932/p/all/c888/47950",
      "/t/6391/n1/all/v/5932/p/all/c888/56622",
      "/t/6391/n1/all/v/5932/p/all/c888/56623",
      "/t/6391/n1/all/v/5932/p/all/c888/56624",
      "/t/6391/n1/all/v/5932/p/all/c888/60032",
      "/t/6391/n1/all/v/5932/p/all/c888/56627",
      "/t/6391/n1/all/v/5932/p/all/c888/56628",
      "/t/6392/n1/all/v/6293/p/all",
      "/t/6393/n1/all/v/6295/p/all"
    ),
    table_id = c(
      6390L, 6387L, 6389L, 6388L,
      rep(6389L, 17),
      rep(6391L, 10),
      6392L, 6393L
    ),
    variable_id = c(
      5933L, 5935L, 5932L, 5934L,
      rep(5932L, 17),
      rep(5932L, 10),
      6293L, 6295L
    ),
    classification_id = c(
      NA_character_, NA_character_, "c11913", NA_character_,
      rep("c11913", 17),
      rep("c888", 10),
      NA_character_, NA_character_
    ),
    classification_value = c(
      NA_character_, NA_character_, "96165", NA_character_,
      "96166", "31721", "31722", "31723",
      "31724", "31725", "31726",
      "31727", "31728", "31729", "31730",
      "96170", "45934", "45935",
      "96171", "45936", "45937",
      "47947", "47948", "47949", "47950", "56622",
      "56623", "56624", "60032", "56627", "56628",
      NA_character_, NA_character_
    ),
    category = "income_real",
    description_pt = c(
      "Rendimento habitual real medio de todos os trabalhos",
      "Rendimento efetivo real medio de todos os trabalhos",
      "Rendimento habitual real medio do trabalho principal",
      "Rendimento efetivo real medio do trabalho principal",
      "Rend. hab. real principal - Empregado",
      "Rend. hab. real principal - Empregado setor privado",
      "Rend. hab. real principal - Empregado setor privado com carteira",
      "Rend. hab. real principal - Empregado setor privado sem carteira",
      "Rend. hab. real principal - Trabalhador domestico",
      "Rend. hab. real principal - Trabalhador domestico com carteira",
      "Rend. hab. real principal - Trabalhador domestico sem carteira",
      "Rend. hab. real principal - Empregado setor publico",
      "Rend. hab. real principal - Empregado setor publico com carteira",
      "Rend. hab. real principal - Empregado setor publico sem carteira",
      "Rend. hab. real principal - Militar e servidor estatutario",
      "Rend. hab. real principal - Empregador",
      "Rend. hab. real principal - Empregador com CNPJ",
      "Rend. hab. real principal - Empregador sem CNPJ",
      "Rend. hab. real principal - Conta propria",
      "Rend. hab. real principal - Conta propria com CNPJ",
      "Rend. hab. real principal - Conta propria sem CNPJ",
      "Rend. hab. real principal - Agropecuaria",
      "Rend. hab. real principal - Industria",
      "Rend. hab. real principal - Construcao",
      "Rend. hab. real principal - Comercio",
      "Rend. hab. real principal - Transporte, armazenagem e correio",
      "Rend. hab. real principal - Alojamento e alimentacao",
      "Rend. hab. real principal - Informacao, comunicacao, financeiras, imobiliarias e administrativas",
      "Rend. hab. real principal - Administracao publica, defesa, seguridade, educacao, saude e servicos sociais",
      "Rend. hab. real principal - Outros servicos",
      "Rend. hab. real principal - Servicos domesticos",
      "Massa de rendimento habitual real de todos os trabalhos",
      "Massa de rendimento efetivo real de todos os trabalhos"
    ),
    unit = c(
      "currency", "currency", "currency", "currency",
      rep("currency", 27),
      "currency_millions", "currency_millions"
    ),
    is_derived = FALSE,
    requires_deflation = TRUE
  )

  # ============================================================================
  # CATEGORY: UNDERUTILIZATION (subutilizacao da forca de trabalho)
  # ============================================================================

  underutilization <- data.table::data.table(
    series_name = c(
      "contribuinteprev", "subocuphoras", "forcapotencial", "forcaampliada", "desalentado",
      "perccontribprev", "taxacombdesosub", "taxacombdesopot", "taxacompsubutlz",
      "taxasubocuphoras", "percdesalento"
    ),
    api_path = c(
      "/t/3918/n1/all/v/4090/p/all/c12027/allxt",
      "/t/6438/n1/all/v/1641/p/all/c604/31751",
      "/t/6438/n1/all/v/1641/p/all/c604/31752",
      "/t/6438/n1/all/v/1641/p/all/c604/40287",
      "/t/6438/n1/all/v/1641/p/all/c604/46254",
      "/t/3919/n1/all/v/8463/p/all/d/v8463%201",
      "/t/6439/n1/all/v/4114/p/all/d/v4114%201",
      "/t/6440/n1/all/v/4116/p/all/d/v4116%201",
      "/t/6441/n1/all/v/4118/p/all/d/v4118%201",
      "/t/6785/n1/all/v/9819/p/all/d/v9819%201",
      "/t/6807/n1/all/v/9869/p/all/d/v9869%201"
    ),
    table_id = c(3918L, 6438L, 6438L, 6438L, 6438L, 3919L, 6439L, 6440L, 6441L, 6785L, 6807L),
    variable_id = c(4090L, 1641L, 1641L, 1641L, 1641L, 8463L, 4114L, 4116L, 4118L, 9819L, 9869L),
    classification_id = c("c12027", "c604", "c604", "c604", "c604", rep(NA_character_, 6)),
    classification_value = c("allxt", "31751", "31752", "40287", "46254", rep(NA_character_, 6)),
    category = "underutilization",
    description_pt = c(
      "Contribuintes para a previdencia",
      "Subocupados por insuficiencia de horas",
      "Forca de trabalho potencial",
      "Forca de trabalho ampliada",
      "Desalentados",
      "Percentual de contribuintes para a previdencia",
      "Taxa combinada de desocupacao e subocupacao",
      "Taxa combinada de desocupacao e forca potencial",
      "Taxa composta de subutilizacao",
      "Taxa de subocupacao por insuficiencia de horas",
      "Percentual de desalentados na forca potencial"
    ),
    unit = c(rep("thousands", 5), rep("rate", 6)),
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # CATEGORY: PRICE INDICES (for deflation)
  # ============================================================================

  price_index <- data.table::data.table(
    series_name = c("ipca100dez1993", "ipcavarmensal", "inpc100dez1993", "inpcvarmensal"),
    api_path = c(
      "/t/1737/n1/all/v/2266/p/all/d/v2266%2013",
      "/t/1737/n1/all/v/63/p/all/d/v63%202",
      "/t/1736/n1/all/v/2289/p/all/d/v2289%2013",
      "/t/1736/n1/all/v/44/p/all/d/v44%202"
    ),
    table_id = c(1737L, 1737L, 1736L, 1736L),
    variable_id = c(2266L, 63L, 2289L, 44L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    category = "price_index",
    description_pt = c(
      "IPCA - indice (base dez/1993 = 100)",
      "IPCA - variacao mensal (%)",
      "INPC - indice (base dez/1993 = 100)",
      "INPC - variacao mensal (%)"
    ),
    unit = c("index", "rate", "index", "rate"),
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # Combine all categories
  # ============================================================================

  meta <- data.table::rbindlist(list(
    rates, population, employment, sectors,
    income_nominal, income_real, underutilization, price_index
  ), fill = TRUE)

  # Filter by category if specified
  if (!is.null(category)) {
    valid_categories <- c("rate", "population", "employment", "sector",
                          "income_nominal", "income_real", "underutilization", "price_index")
    invalid <- setdiff(category, valid_categories)
    if (length(invalid) > 0) {
      stop("Invalid category: ", paste(invalid, collapse = ", "),
           ". Valid categories: ", paste(valid_categories, collapse = ", "))
    }
    # Use local variable to avoid conflict with column name
    filter_categories <- category
    meta <- meta[get("category") %in% filter_categories]
  }

  # Filter by series if specified
  if (!identical(series, "all")) {
    invalid <- setdiff(series, meta$series_name)
    if (length(invalid) > 0) {
      stop("Unknown series: ", paste(invalid, collapse = ", "))
    }
    meta <- meta[series_name %in% series]
  }

  return(meta)
}

#' List Available SIDRA Series Names
#'
#' Returns a character vector of all available series names that can be
#' fetched and mensalized.
#'
#' @param category Optional character vector of categories to filter by.
#'
#' @return Character vector of series names.
#'
#' @examples
#' \dontrun{
#' # All series
#' list_sidra_series()
#'
#' # Only population series
#' list_sidra_series(category = "population")
#' }
#'
#' @export
list_sidra_series <- function(category = NULL) {
  meta <- get_sidra_series_metadata(category = category)
  return(meta$series_name)
}

#' Get Series Categories
#'
#' Returns a list of valid series categories for filtering.
#'
#' @return Character vector of valid category names.
#'
#' @export
get_sidra_categories <- function() {
  c("rate", "population", "employment", "sector",
    "income_nominal", "income_real", "underutilization", "price_index")
}


# =============================================================================
# Internal Helper Functions
# =============================================================================

#' Get Primary Series Names (for mensalization)
#'
#' Returns series that can be directly mensalized (not derived from others).
#' These are population counts, employment counts, sector counts, and
#' some income series.
#'
#' @return Character vector of primary series names.
#'
#' @keywords internal
.get_primary_series <- function() {
  meta <- get_sidra_series_metadata()
  meta[unit %in% c("thousands", "currency", "currency_millions") &
         is_derived == FALSE, series_name]
}

#' Get Rate Series Names
#'
#' Returns series that are rates (typically derived from primary series).
#'
#' @return Character vector of rate series names.
#'
#' @keywords internal
.get_rate_series <- function() {
  meta <- get_sidra_series_metadata()
  meta[unit == "rate", series_name]
}

#' Get Series Requiring IPCA Deflation
#'
#' Returns series that need IPCA deflation for proper mensalization.
#'
#' @return Character vector of series names requiring deflation.
#'
#' @keywords internal
.get_deflation_series <- function() {
  meta <- get_sidra_series_metadata()
  meta[requires_deflation == TRUE, series_name]
}

#' Map SIDRA Period String to YYYYMM Format
#'
#' Converts SIDRA period codes (e.g., "202301 janeiro - marco 2023") to
#' anomesfinaltrimmovel format (YYYYMM of quarter end month).
#'
#' @param period_string Character. SIDRA period description.
#'
#' @return Integer. YYYYMM format of quarter end month.
#'
#' @keywords internal
.parse_sidra_period <- function(period_string) {
  # SIDRA returns periods like "202301 janeiro - marco 2023"

# We need to extract the end month
  # The period code format is YYYYMM where MM is the end month
  as.integer(substr(period_string, 1, 6))
}

#' Get Month Position in Quarter (mesnotrim)
#'
#' Given a month (1-12), returns its position in the rolling quarter (1, 2, or 3).
#' - January, April, July, October = position 1
#' - February, May, August, November = position 2
#' - March, June, September, December = position 3
#'
#' @param month Integer. Month number (1-12).
#'
#' @return Integer. Position in quarter (1, 2, or 3).
#'
#' @keywords internal
.get_mesnotrim <- function(month) {
  ((month - 1) %% 3) + 1
}
