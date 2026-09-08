# Read at app startup so maintainers can revise choices without editing app.R.
read_survey_choices <- function(path = system.file(
  "extdata", "survey_choices.csv", package = "micsPlusTableR"
)) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("The packaged extdata/survey_choices.csv file is missing.", call. = FALSE)
  }

  choices <- utils::read.csv(
    path, colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
  )
  required <- c("country", "country_code", "period", "wave")
  if (!all(required %in% names(choices)) || anyDuplicated(names(choices))) {
    stop("survey_choices.csv must contain country, country_code, period, and wave columns.",
         call. = FALSE)
  }
  choices <- choices[required]
  choices[] <- lapply(choices, trimws)
  if (!nrow(choices) || anyNA(choices) || any(choices == "")) {
    stop("survey_choices.csv must contain at least one row with no empty fields.",
         call. = FALSE)
  }
  if (any(!grepl("^[A-Z]{3}$", choices$country_code))) {
    stop("survey_choices.csv country_code values must be three uppercase letters (for example JAM).",
         call. = FALSE)
  }

  choices$label <- paste0(choices$country, " (", choices$period, ")")
  surveys <- unique(choices[c("label", "country", "country_code", "period")])
  if (anyDuplicated(surveys$label)) {
    stop("survey_choices.csv must use one country code per country and period.",
         call. = FALSE)
  }
  if (anyDuplicated(choices[c("label", "wave")])) {
    stop("survey_choices.csv contains duplicate country, period, and wave rows.",
         call. = FALSE)
  }
  choices
}
