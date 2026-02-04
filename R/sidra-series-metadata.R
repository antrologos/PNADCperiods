#' SIDRA Series Metadata for PNADC Mensalization
#'
#' This file contains metadata definitions for all 86 SIDRA series used in the
#' mensalization process. It maps series names to their SIDRA API endpoints,
#' table IDs, variable codes, and hierarchical categorization.
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
#' @param theme Character vector of themes to filter by. Valid themes:
#'   "labor_market", "earnings", "demographics", "social_protection", "prices".
#'   Use NULL (default) for no filtering.
#' @param theme_category Character vector of theme categories to filter by.
#'   Use NULL (default) for no filtering.
#' @param subcategory Character vector of subcategories to filter by.
#'   Use NULL (default) for no filtering.
#' @param lang Character. Language for descriptions: "pt" (Portuguese, default)
#'   or "en" (English). When "en", the `description` column contains English text.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{series_name}{Character. Internal name used in the package}
#'     \item{api_path}{Character. SIDRA API path for get_sidra()}
#'     \item{table_id}{Integer. SIDRA table number}
#'     \item{variable_id}{Integer. SIDRA variable code}
#'     \item{theme}{Character. Top-level theme}
#'     \item{theme_category}{Character. Middle-level category within theme}
#'     \item{subcategory}{Character. Optional subcategory for filtering}
#'     \item{description_pt}{Character. Portuguese description}
#'     \item{description_en}{Character. English description}
#'     \item{description}{Character. Description in the requested language}
#'     \item{unit}{Character. Unit of measurement}
#'     \item{unit_label_pt}{Character. Unit label in Portuguese}
#'     \item{unit_label_en}{Character. Unit label in English}
#'     \item{is_derived}{Logical. TRUE if computed from other series}
#'     \item{requires_deflation}{Logical. TRUE if needs IPCA deflation}
#'   }
#'
#' @examples
#' \dontrun{
#' # Get all series metadata
#' meta <- get_sidra_series_metadata()
#'
#' # Get only labor market series
#' labor <- get_sidra_series_metadata(theme = "labor_market")
#'
#' # Get unemployment rates
#' unemp <- get_sidra_series_metadata(theme = "labor_market",
#'                                     theme_category = "unemployment")
#'
#' # Get specific series with English descriptions
#' meta <- get_sidra_series_metadata(series = c("taxadesocup", "popocup"),
#'                                    lang = "en")
#' }
#'
#' @export
get_sidra_series_metadata <- function(series = "all",
                                       theme = NULL,
                                       theme_category = NULL,
                                       subcategory = NULL,
                                       lang = "pt") {

  # Build complete metadata table

  meta <- data.table::data.table(
    series_name = character(),
    api_path = character(),
    table_id = integer(),
    variable_id = integer(),
    classification_id = character(),
    classification_value = character(),
    theme = character(),
    theme_category = character(),
    subcategory = character(),
    description_pt = character(),
    description_en = character(),
    unit = character(),
    unit_label_pt = character(),
    unit_label_en = character(),
    is_derived = logical(),
    requires_deflation = logical()
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Participation (Rates)
  # ============================================================================

  participation_rates <- data.table::data.table(
    series_name = c("taxapartic", "nivelocup", "niveldesocup"),
    api_path = c(
      "/t/5944/n1/all/v/4096/p/all/d/v4096%201",
      "/t/6379/n1/all/v/4097/p/all/d/v4097%201",
      "/t/6380/n1/all/v/4098/p/all/d/v4098%201"
    ),
    table_id = c(5944L, 6379L, 6380L),
    variable_id = c(4096L, 4097L, 4098L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "labor_market",
    theme_category = "participation",
    subcategory = "rates",
    description_pt = c(
      "Taxa de participacao na forca de trabalho",
      "Nivel de ocupacao",
      "Nivel de desocupacao"
    ),
    description_en = c(
      "Labor force participation rate",
      "Employment-population ratio",
      "Unemployment-population ratio"
    ),
    unit = "percent",
    unit_label_pt = "%",
    unit_label_en = "%",
    is_derived = TRUE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Participation (Levels)
  # ============================================================================

  participation_levels <- data.table::data.table(
    series_name = c("popnaforca", "popocup", "popdesocup", "popforadaforca"),
    api_path = c(
      "/t/6318/n1/all/v/1641/p/all/c629/32386",
      "/t/6318/n1/all/v/1641/p/all/c629/32387",
      "/t/6318/n1/all/v/1641/p/all/c629/32446",
      "/t/6318/n1/all/v/1641/p/all/c629/32447"
    ),
    table_id = 6318L,
    variable_id = 1641L,
    classification_id = "c629",
    classification_value = c("32386", "32387", "32446", "32447"),
    theme = "labor_market",
    theme_category = "participation",
    subcategory = "levels",
    description_pt = c(
      "Populacao na forca de trabalho",
      "Populacao ocupada",
      "Populacao desocupada",
      "Populacao fora da forca de trabalho"
    ),
    description_en = c(
      "Labor force",
      "Employed population",
      "Unemployed population",
      "Population not in labor force"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Unemployment (Rates)
  # ============================================================================

  unemployment_rates <- data.table::data.table(
    series_name = "taxadesocup",
    api_path = "/t/6381/n1/all/v/4099/p/all/d/v4099%201",
    table_id = 6381L,
    variable_id = 4099L,
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "labor_market",
    theme_category = "unemployment",
    subcategory = "rates",
    description_pt = "Taxa de desocupacao",
    description_en = "Unemployment rate",
    unit = "percent",
    unit_label_pt = "%",
    unit_label_en = "%",
    is_derived = TRUE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Underutilization (Rates)
  # ============================================================================

  underutilization_rates <- data.table::data.table(
    series_name = c("perccontribprev", "taxacombdesosub", "taxacombdesopot",
                    "taxacompsubutlz", "taxasubocuphoras", "percdesalento"),
    api_path = c(
      "/t/3919/n1/all/v/8463/p/all/d/v8463%201",
      "/t/6439/n1/all/v/4114/p/all/d/v4114%201",
      "/t/6440/n1/all/v/4116/p/all/d/v4116%201",
      "/t/6441/n1/all/v/4118/p/all/d/v4118%201",
      "/t/6785/n1/all/v/9819/p/all/d/v9819%201",
      "/t/6807/n1/all/v/9869/p/all/d/v9869%201"
    ),
    table_id = c(3919L, 6439L, 6440L, 6441L, 6785L, 6807L),
    variable_id = c(8463L, 4114L, 4116L, 4118L, 9819L, 9869L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "labor_market",
    theme_category = "underutilization",
    subcategory = "rates",
    description_pt = c(
      "Percentual de contribuintes para a previdencia",
      "Taxa combinada de desocupacao e subocupacao",
      "Taxa combinada de desocupacao e forca potencial",
      "Taxa composta de subutilizacao",
      "Taxa de subocupacao por insuficiencia de horas",
      "Percentual de desalentados na forca potencial"
    ),
    description_en = c(
      "Social security contribution rate",
      "Combined unemployment and underemployment rate",
      "Combined unemployment and potential labor force rate",
      "Composite underutilization rate",
      "Time-related underemployment rate",
      "Discouraged workers as share of potential labor force"
    ),
    unit = "percent",
    unit_label_pt = "%",
    unit_label_en = "%",
    is_derived = TRUE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Underutilization (Levels)
  # ============================================================================

  underutilization_levels <- data.table::data.table(
    series_name = c("subocuphoras", "forcapotencial", "forcaampliada", "desalentado"),
    api_path = c(
      "/t/6438/n1/all/v/1641/p/all/c604/31751",
      "/t/6438/n1/all/v/1641/p/all/c604/31752",
      "/t/6438/n1/all/v/1641/p/all/c604/40287",
      "/t/6438/n1/all/v/1641/p/all/c604/46254"
    ),
    table_id = 6438L,
    variable_id = 1641L,
    classification_id = "c604",
    classification_value = c("31751", "31752", "40287", "46254"),
    theme = "labor_market",
    theme_category = "underutilization",
    subcategory = "levels",
    description_pt = c(
      "Subocupados por insuficiencia de horas",
      "Forca de trabalho potencial",
      "Forca de trabalho ampliada",
      "Desalentados"
    ),
    description_en = c(
      "Time-related underemployed",
      "Potential labor force",
      "Extended labor force",
      "Discouraged workers"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Employment Type (Employees)
  # ============================================================================

  employment_employees <- data.table::data.table(
    series_name = c(
      "empregado", "empregpriv", "empregprivcomcart", "empregprivsemcart",
      "empregpubl", "empregpublcomcart", "empregpublsemcart", "estatutmilitar"
    ),
    api_path = c(
      "/t/6320/n1/all/v/4090/p/all/c11913/96166",
      "/t/6320/n1/all/v/4090/p/all/c11913/31721",
      "/t/6320/n1/all/v/4090/p/all/c11913/31722",
      "/t/6320/n1/all/v/4090/p/all/c11913/31723",
      "/t/6320/n1/all/v/4090/p/all/c11913/31727",
      "/t/6320/n1/all/v/4090/p/all/c11913/31728",
      "/t/6320/n1/all/v/4090/p/all/c11913/31729",
      "/t/6320/n1/all/v/4090/p/all/c11913/31730"
    ),
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = c("96166", "31721", "31722", "31723", "31727", "31728", "31729", "31730"),
    theme = "labor_market",
    theme_category = "employment_type",
    subcategory = "employees",
    description_pt = c(
      "Empregado (total)",
      "Empregado no setor privado",
      "Empregado no setor privado com carteira",
      "Empregado no setor privado sem carteira",
      "Empregado no setor publico",
      "Empregado no setor publico com carteira",
      "Empregado no setor publico sem carteira",
      "Militar e servidor estatutario"
    ),
    description_en = c(
      "Employees (total)",
      "Private sector employees",
      "Private sector employees with formal contract",
      "Private sector employees without formal contract",
      "Public sector employees",
      "Public sector employees with formal contract",
      "Public sector employees without formal contract",
      "Military and statutory civil servants"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Employment Type (Domestic Workers)
  # ============================================================================

  employment_domestic <- data.table::data.table(
    series_name = c("domestico", "domesticocomcart", "domesticosemcart"),
    api_path = c(
      "/t/6320/n1/all/v/4090/p/all/c11913/31724",
      "/t/6320/n1/all/v/4090/p/all/c11913/31725",
      "/t/6320/n1/all/v/4090/p/all/c11913/31726"
    ),
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = c("31724", "31725", "31726"),
    theme = "labor_market",
    theme_category = "employment_type",
    subcategory = "domestic",
    description_pt = c(
      "Trabalhador domestico",
      "Trabalhador domestico com carteira",
      "Trabalhador domestico sem carteira"
    ),
    description_en = c(
      "Domestic workers",
      "Domestic workers with formal contract",
      "Domestic workers without formal contract"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Employment Type (Employers)
  # ============================================================================

  employment_employers <- data.table::data.table(
    series_name = c("empregador", "empregadorcomcnpj", "empregadorsemcnpj"),
    api_path = c(
      "/t/6320/n1/all/v/4090/p/all/c11913/96170",
      "/t/6320/n1/all/v/4090/p/all/c11913/45934",
      "/t/6320/n1/all/v/4090/p/all/c11913/45935"
    ),
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = c("96170", "45934", "45935"),
    theme = "labor_market",
    theme_category = "employment_type",
    subcategory = "employers",
    description_pt = c(
      "Empregador (total)",
      "Empregador com CNPJ",
      "Empregador sem CNPJ"
    ),
    description_en = c(
      "Employers (total)",
      "Employers with business registration (CNPJ)",
      "Employers without business registration"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Employment Type (Self-Employed)
  # ============================================================================

  employment_self_employed <- data.table::data.table(
    series_name = c("contapropria", "contapropriacomcnpj", "contapropriasemcnpj"),
    api_path = c(
      "/t/6320/n1/all/v/4090/p/all/c11913/96171",
      "/t/6320/n1/all/v/4090/p/all/c11913/45936",
      "/t/6320/n1/all/v/4090/p/all/c11913/45937"
    ),
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = c("96171", "45936", "45937"),
    theme = "labor_market",
    theme_category = "employment_type",
    subcategory = "self_employed",
    description_pt = c(
      "Conta propria (total)",
      "Conta propria com CNPJ",
      "Conta propria sem CNPJ"
    ),
    description_en = c(
      "Self-employed (total)",
      "Self-employed with business registration (CNPJ)",
      "Self-employed without business registration"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Employment Type (Family Workers)
  # ============================================================================

  employment_family <- data.table::data.table(
    series_name = "trabfamauxiliar",
    api_path = "/t/6320/n1/all/v/4090/p/all/c11913/31731",
    table_id = 6320L,
    variable_id = 4090L,
    classification_id = "c11913",
    classification_value = "31731",
    theme = "labor_market",
    theme_category = "employment_type",
    subcategory = "family_workers",
    description_pt = "Trabalhador familiar auxiliar",
    description_en = "Unpaid family workers",
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: LABOR_MARKET - Economic Sector
  # ============================================================================

  economic_sector <- data.table::data.table(
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
    theme = "labor_market",
    theme_category = "economic_sector",
    subcategory = NA_character_,
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
    description_en = c(
      "Employed in agriculture",
      "Employed in industry",
      "Employed in construction",
      "Employed in commerce",
      "Employed in transport, storage and mail",
      "Employed in accommodation and food services",
      "Employed in information, communication, finance, real estate and admin",
      "Employed in public admin, defense, social security, education, health and social services",
      "Employed in other services",
      "Employed in domestic services"
    ),
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: EARNINGS - Average Usual (All Jobs)
  # ============================================================================

  earnings_usual_all <- data.table::data.table(
    series_name = c("rendhabnominaltodos", "rendhabrealtodos",
                    "rendhabrealprinc", "rendefetrealprinc"),
    api_path = c(
      "/t/6390/n1/all/v/5929/p/all",
      "/t/6390/n1/all/v/5933/p/all",
      "/t/6389/n1/all/v/5932/p/all/c11913/96165",
      "/t/6388/n1/all/v/5934/p/all"
    ),
    table_id = c(6390L, 6390L, 6389L, 6388L),
    variable_id = c(5929L, 5933L, 5932L, 5934L),
    classification_id = c(NA_character_, NA_character_, "c11913", NA_character_),
    classification_value = c(NA_character_, NA_character_, "96165", NA_character_),
    theme = "earnings",
    theme_category = "average_usual",
    subcategory = "all_jobs",
    description_pt = c(
      "Rendimento habitual nominal medio de todos os trabalhos",
      "Rendimento habitual real medio de todos os trabalhos",
      "Rendimento habitual real medio do trabalho principal",
      "Rendimento efetivo real medio do trabalho principal"
    ),
    description_en = c(
      "Average usual nominal earnings from all jobs",
      "Average usual real earnings from all jobs",
      "Average usual real earnings from main job",
      "Average effective real earnings from main job"
    ),
    unit = "currency",
    unit_label_pt = "R$",
    unit_label_en = "BRL",
    is_derived = FALSE,
    requires_deflation = c(FALSE, TRUE, TRUE, TRUE)
  )

  # ============================================================================
  # THEME: EARNINGS - Average Usual (By Employment Type)
  # ============================================================================

  earnings_usual_by_type <- data.table::data.table(
    series_name = c(
      "rhrpempregado", "rhrpempregpriv", "rhrpempregprivcomcart", "rhrpempregprivsemcart",
      "rhrpdomestico", "rhrpdomesticocomcart", "rhrpdomesticosemcart",
      "rhrpempregpubl", "rhrpempregpublcomcart", "rhrpempregpublsemcart", "rhrpestatutmilitar",
      "rhrpempregador", "rhrpempregadorcomcnpj", "rhrpempregadorsemcnpj",
      "rhrpcontapropria", "rhrpcontapropriacomcnpj", "rhrpcontapropriasemcnpj"
    ),
    api_path = c(
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
      "/t/6389/n1/all/v/5932/p/all/c11913/45937"
    ),
    table_id = 6389L,
    variable_id = 5932L,
    classification_id = "c11913",
    classification_value = c(
      "96166", "31721", "31722", "31723",
      "31724", "31725", "31726",
      "31727", "31728", "31729", "31730",
      "96170", "45934", "45935",
      "96171", "45936", "45937"
    ),
    theme = "earnings",
    theme_category = "average_usual",
    subcategory = "by_employment_type",
    description_pt = c(
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
      "Rend. hab. real principal - Conta propria sem CNPJ"
    ),
    description_en = c(
      "Usual real earnings (main job) - Employees",
      "Usual real earnings (main job) - Private sector employees",
      "Usual real earnings (main job) - Private sector with formal contract",
      "Usual real earnings (main job) - Private sector without formal contract",
      "Usual real earnings (main job) - Domestic workers",
      "Usual real earnings (main job) - Domestic workers with formal contract",
      "Usual real earnings (main job) - Domestic workers without formal contract",
      "Usual real earnings (main job) - Public sector employees",
      "Usual real earnings (main job) - Public sector with formal contract",
      "Usual real earnings (main job) - Public sector without formal contract",
      "Usual real earnings (main job) - Military and statutory servants",
      "Usual real earnings (main job) - Employers",
      "Usual real earnings (main job) - Employers with CNPJ",
      "Usual real earnings (main job) - Employers without CNPJ",
      "Usual real earnings (main job) - Self-employed",
      "Usual real earnings (main job) - Self-employed with CNPJ",
      "Usual real earnings (main job) - Self-employed without CNPJ"
    ),
    unit = "currency",
    unit_label_pt = "R$",
    unit_label_en = "BRL",
    is_derived = FALSE,
    requires_deflation = TRUE
  )

  # ============================================================================
  # THEME: EARNINGS - Average Usual (By Economic Sector)
  # ============================================================================

  earnings_usual_by_sector <- data.table::data.table(
    series_name = c(
      "rhrpagropecuaria", "rhrpindustria", "rhrpconstrucao", "rhrpcomercio", "rhrptransporte",
      "rhrpalojaliment", "rhrpinfcomfinimobadm", "rhrpadminpublica", "rhrpoutroservico", "rhrpservicodomestico"
    ),
    api_path = c(
      "/t/6391/n1/all/v/5932/p/all/c888/47947",
      "/t/6391/n1/all/v/5932/p/all/c888/47948",
      "/t/6391/n1/all/v/5932/p/all/c888/47949",
      "/t/6391/n1/all/v/5932/p/all/c888/47950",
      "/t/6391/n1/all/v/5932/p/all/c888/56622",
      "/t/6391/n1/all/v/5932/p/all/c888/56623",
      "/t/6391/n1/all/v/5932/p/all/c888/56624",
      "/t/6391/n1/all/v/5932/p/all/c888/60032",
      "/t/6391/n1/all/v/5932/p/all/c888/56627",
      "/t/6391/n1/all/v/5932/p/all/c888/56628"
    ),
    table_id = 6391L,
    variable_id = 5932L,
    classification_id = "c888",
    classification_value = c(
      "47947", "47948", "47949", "47950", "56622",
      "56623", "56624", "60032", "56627", "56628"
    ),
    theme = "earnings",
    theme_category = "average_usual",
    subcategory = "by_economic_sector",
    description_pt = c(
      "Rend. hab. real principal - Agropecuaria",
      "Rend. hab. real principal - Industria",
      "Rend. hab. real principal - Construcao",
      "Rend. hab. real principal - Comercio",
      "Rend. hab. real principal - Transporte, armazenagem e correio",
      "Rend. hab. real principal - Alojamento e alimentacao",
      "Rend. hab. real principal - Informacao, comunicacao, financeiras, imobiliarias e administrativas",
      "Rend. hab. real principal - Administracao publica, defesa, seguridade, educacao, saude e servicos sociais",
      "Rend. hab. real principal - Outros servicos",
      "Rend. hab. real principal - Servicos domesticos"
    ),
    description_en = c(
      "Usual real earnings (main job) - Agriculture",
      "Usual real earnings (main job) - Industry",
      "Usual real earnings (main job) - Construction",
      "Usual real earnings (main job) - Commerce",
      "Usual real earnings (main job) - Transport, storage and mail",
      "Usual real earnings (main job) - Accommodation and food services",
      "Usual real earnings (main job) - Information, communication, finance, real estate and admin",
      "Usual real earnings (main job) - Public admin, defense, education, health and social services",
      "Usual real earnings (main job) - Other services",
      "Usual real earnings (main job) - Domestic services"
    ),
    unit = "currency",
    unit_label_pt = "R$",
    unit_label_en = "BRL",
    is_derived = FALSE,
    requires_deflation = TRUE
  )

  # ============================================================================
  # THEME: EARNINGS - Average Effective (All Jobs)
  # ============================================================================

  earnings_effective_all <- data.table::data.table(
    series_name = c("rendefetnominaltodos", "rendefetrealtodos"),
    api_path = c(
      "/t/6387/n1/all/v/5931/p/all",
      "/t/6387/n1/all/v/5935/p/all"
    ),
    table_id = 6387L,
    variable_id = c(5931L, 5935L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "earnings",
    theme_category = "average_effective",
    subcategory = "all_jobs",
    description_pt = c(
      "Rendimento efetivo nominal medio de todos os trabalhos",
      "Rendimento efetivo real medio de todos os trabalhos"
    ),
    description_en = c(
      "Average effective nominal earnings from all jobs",
      "Average effective real earnings from all jobs"
    ),
    unit = "currency",
    unit_label_pt = "R$",
    unit_label_en = "BRL",
    is_derived = FALSE,
    requires_deflation = c(FALSE, TRUE)
  )

  # ============================================================================
  # THEME: EARNINGS - Wage Mass (Usual)
  # ============================================================================

  wage_mass_usual <- data.table::data.table(
    series_name = c("massahabnominaltodos", "massahabrealtodos"),
    api_path = c(
      "/t/6392/n1/all/v/6288/p/all",
      "/t/6392/n1/all/v/6293/p/all"
    ),
    table_id = 6392L,
    variable_id = c(6288L, 6293L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "earnings",
    theme_category = "wage_mass",
    subcategory = "usual",
    description_pt = c(
      "Massa de rendimento habitual nominal de todos os trabalhos",
      "Massa de rendimento habitual real de todos os trabalhos"
    ),
    description_en = c(
      "Usual nominal wage mass from all jobs",
      "Usual real wage mass from all jobs"
    ),
    unit = "currency_millions",
    unit_label_pt = "R$ milhoes",
    unit_label_en = "BRL millions",
    is_derived = FALSE,
    requires_deflation = c(FALSE, TRUE)
  )

  # ============================================================================
  # THEME: EARNINGS - Wage Mass (Effective)
  # ============================================================================

  wage_mass_effective <- data.table::data.table(
    series_name = c("massaefetnominaltodos", "massaefetrealtodos"),
    api_path = c(
      "/t/6393/n1/all/v/6291/p/all",
      "/t/6393/n1/all/v/6295/p/all"
    ),
    table_id = 6393L,
    variable_id = c(6291L, 6295L),
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "earnings",
    theme_category = "wage_mass",
    subcategory = "effective",
    description_pt = c(
      "Massa de rendimento efetivo nominal de todos os trabalhos",
      "Massa de rendimento efetivo real de todos os trabalhos"
    ),
    description_en = c(
      "Effective nominal wage mass from all jobs",
      "Effective real wage mass from all jobs"
    ),
    unit = "currency_millions",
    unit_label_pt = "R$ milhoes",
    unit_label_en = "BRL millions",
    is_derived = FALSE,
    requires_deflation = c(FALSE, TRUE)
  )

  # ============================================================================
  # THEME: DEMOGRAPHICS - Total Population
  # ============================================================================

  demographics_total <- data.table::data.table(
    series_name = "populacao",
    api_path = "/t/6022/n1/all/v/606/p/all",
    table_id = 6022L,
    variable_id = 606L,
    classification_id = NA_character_,
    classification_value = NA_character_,
    theme = "demographics",
    theme_category = "total",
    subcategory = NA_character_,
    description_pt = "Populacao total residente",
    description_en = "Total resident population",
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: DEMOGRAPHICS - Working Age Population
  # ============================================================================

  demographics_working_age <- data.table::data.table(
    series_name = "pop14mais",
    api_path = "/t/6318/n1/all/v/1641/p/all/c629/32385",
    table_id = 6318L,
    variable_id = 1641L,
    classification_id = "c629",
    classification_value = "32385",
    theme = "demographics",
    theme_category = "working_age",
    subcategory = NA_character_,
    description_pt = "Populacao de 14 anos ou mais de idade",
    description_en = "Working age population (14 years and older)",
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: SOCIAL_PROTECTION - Social Security
  # ============================================================================

  social_security <- data.table::data.table(
    series_name = "contribuinteprev",
    api_path = "/t/3918/n1/all/v/4090/p/all/c12027/allxt",
    table_id = 3918L,
    variable_id = 4090L,
    classification_id = "c12027",
    classification_value = "allxt",
    theme = "social_protection",
    theme_category = "social_security",
    subcategory = NA_character_,
    description_pt = "Contribuintes para a previdencia",
    description_en = "Social security contributors",
    unit = "millions",
    unit_label_pt = "milhoes",
    unit_label_en = "millions",
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # THEME: PRICES - Deflators
  # ============================================================================

  price_deflators <- data.table::data.table(
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
    theme = "prices",
    theme_category = "deflators",
    subcategory = NA_character_,
    description_pt = c(
      "IPCA - indice (base dez/1993 = 100)",
      "IPCA - variacao mensal (%)",
      "INPC - indice (base dez/1993 = 100)",
      "INPC - variacao mensal (%)"
    ),
    description_en = c(
      "IPCA - Consumer Price Index (Dec/1993 = 100)",
      "IPCA - Monthly variation (%)",
      "INPC - National Consumer Price Index (Dec/1993 = 100)",
      "INPC - Monthly variation (%)"
    ),
    unit = c("index", "percent", "index", "percent"),
    unit_label_pt = c("indice", "%", "indice", "%"),
    unit_label_en = c("index", "%", "index", "%"),
    is_derived = FALSE,
    requires_deflation = FALSE
  )

  # ============================================================================
  # Combine all categories
  # ============================================================================

  meta <- data.table::rbindlist(list(
    participation_rates, participation_levels,
    unemployment_rates,
    underutilization_rates, underutilization_levels,
    employment_employees, employment_domestic, employment_employers,
    employment_self_employed, employment_family,
    economic_sector,
    earnings_usual_all, earnings_usual_by_type, earnings_usual_by_sector,
    earnings_effective_all,
    wage_mass_usual, wage_mass_effective,
    demographics_total, demographics_working_age,
    social_security,
    price_deflators
  ), fill = TRUE)

  # Add description column based on language
  if (lang == "en") {
    meta[, description := description_en]
  } else {
    meta[, description := description_pt]
  }

  # ============================================================================
  # Filter by hierarchical criteria
  # ============================================================================

  # Filter by theme if specified
  if (!is.null(theme)) {
    valid_themes <- c("labor_market", "earnings", "demographics", "social_protection", "prices")
    invalid <- setdiff(theme, valid_themes)
    if (length(invalid) > 0) {
      stop("Invalid theme: ", paste(invalid, collapse = ", "),
           ". Valid themes: ", paste(valid_themes, collapse = ", "))
    }
    filter_themes <- theme
    meta <- meta[get("theme") %in% filter_themes]
  }

  # Filter by theme_category if specified
  if (!is.null(theme_category)) {
    filter_categories <- theme_category
    meta <- meta[get("theme_category") %in% filter_categories]
  }

  # Filter by subcategory if specified
  if (!is.null(subcategory)) {
    filter_subcategories <- subcategory
    # Include rows where subcategory matches OR is NA (for series without subcategories)
    meta <- meta[get("subcategory") %in% filter_subcategories | is.na(get("subcategory"))]
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

# =============================================================================
# Internal Helper Functions
# =============================================================================

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


# =============================================================================
# PNADC Survey Date Constants
# =============================================================================
#
# Centralized constants for PNADC survey dates to ensure consistency across
# all mensalization functions. These reflect IBGE methodology changes.
#
# Usage: Access via .PNADC_DATES$<name>
# =============================================================================

.PNADC_DATES <- list(
  # ==========================================================================
  # Survey inception
  # ==========================================================================
  PNADC_START = 201201L,         # First PNADC month (Jan 2012)

  # ==========================================================================
  # Variable transitions
  # ==========================================================================
  VD4004_SPLIT = 201509L,        # Last month using VD4004 (underemployment)
                                  # VD4004A used from 201510 onwards
  V4019_AVAILABLE = 201510L,     # First month V4019 (CNPJ status) available

  # ==========================================================================
  # Calibration periods
  # ==========================================================================
  DEFAULT_CALIB_START = 201301L, # Default calibration period start
  DEFAULT_CALIB_END = 201912L,   # Default calibration period end (pre-COVID)
  CNPJ_CALIB_START = 201601L,    # Calibration start for CNPJ series
  PRESPLIT_CALIB_END = 201412L   # Calibration end for pre-VD4004A period
)
