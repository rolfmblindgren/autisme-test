## autisme-stats.R
## Analyse av autisme-testen i wide-format.
## Leser først lokal SQLite hvis den finnes, ellers via SSH.

## -------------------------
## 1. Pakker
## -------------------------
needed_packages <- c("psych", "mirt", "DBI", "RSQLite")
for (package_name in needed_packages) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop("Pakken '", package_name, "' er ikke installert.")
  }
}
library(psych)
library(mirt)
library(DBI)
library(RSQLite)

## -------------------------
## 2. Les inn data
## -------------------------
remote_host <- Sys.getenv("AUTISME_DB_HOST", "vds@dnsgrendel.grendel.no")
remote_db_path <- Sys.getenv("AUTISME_DB_REMOTE_PATH", "/srv/shiny-server/data/autisme.sqlite")
local_db_candidates <- c(
  file.path("/srv/shiny-server/data", Sys.getenv("AUTISME_DB_NAME", "autisme.sqlite")),
  file.path(getwd(), "..", "data", "autisme.sqlite"),
  file.path(getwd(), "data", "autisme.sqlite"),
  file.path(getwd(), "autisme.sqlite"),
  file.path(getwd(), "scripts", "autisme.sqlite")
)
local_db_path <- local_db_candidates[vapply(local_db_candidates, function(path) {
  nzchar(path) && file.exists(path) && !dir.exists(path)
}, logical(1))][1]
if (is.na(local_db_path)) {
  local_db_path <- ""
}

build_wide_sql <- function(table_name, item_ids) {
  item_columns <- vapply(item_ids, function(item_id) {
    sprintf("MAX(CASE WHEN item_id = '%s' THEN score END) AS %s", item_id, item_id)
  }, character(1))

  paste(
    "SELECT",
    "  timestamp,",
    paste0("  ", paste(item_columns, collapse = ",\n  "), ","),
    "  MAX(language) AS language",
    "FROM", table_name,
    "GROUP BY timestamp",
    "ORDER BY timestamp;",
    sep = "\n"
  )
}

read_local_sqlite <- function(path, sql) {
  con <- dbConnect(SQLite(), path, flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "PRAGMA query_only = ON;")
  dbGetQuery(con, sql)
}

read_remote_sqlite <- function(host, path, sql) {
  query_file <- tempfile(fileext = ".sql")
  writeLines(sql, query_file)
  on.exit(unlink(query_file), add = TRUE)

  remote_uri <- sprintf("file:%s?mode=ro&immutable=1", path)
  remote_cmd <- paste("sqlite3 -readonly -header -csv", shQuote(remote_uri))
  shell_cmd <- sprintf(
    "ssh %s %s < %s",
    shQuote(host),
    shQuote(remote_cmd),
    shQuote(query_file)
  )

  output <- system(shell_cmd, intern = TRUE, ignore.stderr = FALSE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "Klarte ikke å lese data fra ",
      host, ":", path, "\n",
      paste(output, collapse = "\n")
    )
  }

  text <- paste(output, collapse = "\n")
  if (!nzchar(trimws(text))) {
    stop("Fikk ingen data fra SQLite-spørringen.")
  }

  read.csv(text = text, stringsAsFactors = FALSE)
}

read_sqlite_table <- function(table_name, item_ids) {
  sql <- build_wide_sql(table_name, item_ids)

  if (nzchar(local_db_path) && file.exists(local_db_path) && !dir.exists(local_db_path)) {
    cat("Leser lokal SQLite:\n", normalizePath(local_db_path), "\n\n", sep = "")
    read_local_sqlite(local_db_path, sql)
  } else {
    cat("Leser remote SQLite via SSH:\n", remote_host, ":", remote_db_path, "\n\n", sep = "")
    read_remote_sqlite(remote_host, remote_db_path, sql)
  }
}

response_item_ids <- paste0("item", 1:13)
responses_df <- read_sqlite_table("responses", response_item_ids)

cat("Leste ", nrow(responses_df), " rader.\n\n", sep = "")

if ("timestamp" %in% names(responses_df)) {
  responses_df$timestamp <- as.POSIXct(responses_df$timestamp)
}
if ("language" %in% names(responses_df)) {
  responses_df$language <- trimws(responses_df$language)
}

responses_matrix <- as.data.frame(lapply(responses_df[, response_item_ids], as.numeric))
analysis_min_rows <- as.integer(Sys.getenv("AUTISME_STATS_MIN_ANALYSIS_ROWS", "20"))

if (is.na(analysis_min_rows) || analysis_min_rows < 2) {
  analysis_min_rows <- 20
}

has_variation <- function(x) {
  length(unique(stats::na.omit(x))) > 1
}

analysis_item_ids <- names(responses_matrix)[vapply(responses_matrix, has_variation, logical(1))]
dropped_item_ids <- setdiff(response_item_ids, analysis_item_ids)
analysis_matrix <- responses_matrix[, analysis_item_ids, drop = FALSE]

## -------------------------
## 3. Deskriptiv statistikk
## -------------------------
cat("Deskriptivstatistikk per item:\n")
descriptive_items <- psych::describe(responses_matrix)
print(descriptive_items)
cat("\n")

## gjennomsnittsskår per person
mean_score <- rowMeans(responses_matrix, na.rm = TRUE)
cat("Deskriptivstatistikk for gjennomsnittsskår:\n")
print(psych::describe(as.data.frame(mean_score)))
cat("\n")

if (length(dropped_item_ids) > 0) {
  cat(
    "Merk: Fjernet items uten variasjon fra intern konsistens/faktoranalyse: ",
    paste(dropped_item_ids, collapse = ", "),
    "\n\n",
    sep = ""
  )
}

if (nrow(responses_matrix) < analysis_min_rows) {
  cat(
    "Merk: Det er bare ",
    nrow(responses_matrix),
    " besvarelser. Cronbachs alfa, faktoranalyse og IRT er derfor slått av til det finnes minst ",
    analysis_min_rows,
    " komplette besvarelser.\n\n",
    sep = ""
  )
}

## -------------------------
## 4. Korrelasjoner
## -------------------------
cat("Korrelasjonsmatrise:\n")
if (ncol(analysis_matrix) >= 2 && nrow(analysis_matrix) >= 2) {
  correlation_matrix <- cor(analysis_matrix, use = "pairwise.complete.obs")
  print(round(correlation_matrix, 2))
} else {
  cat("For få variable items til å lage en korrelasjonsmatrise.\n")
}
cat("\n")

## -------------------------
## 5. Intern konsistens
## -------------------------
alpha_out <- NULL
if (nrow(analysis_matrix) >= analysis_min_rows && ncol(analysis_matrix) >= 2) {
  alpha_out <- psych::alpha(analysis_matrix)
  cat("Cronbachs alfa:\n")
  print(alpha_out$total)
  cat("\n")
} else {
  cat("Cronbachs alfa: hoppet over fordi det er for få eller for lite varierte svar.\n")
  cat("\n")
}

## -------------------------
## 6. Parallel analysis / faktortall
## -------------------------
fa_parallel <- NULL
if (nrow(analysis_matrix) >= analysis_min_rows && ncol(analysis_matrix) >= 3) {
  cat("Parallel analysis (lagrer scree-plot til fil):\n")

  png("scree_plot.png", width = 800, height = 800)
  fa_parallel <- psych::fa.parallel(
    analysis_matrix,
    fm = "ml",
    fa = "fa",
    show.legend = TRUE,
    main = "Parallel Analysis Scree Plots"
  )
  dev.off()

  cat("\nParallel analysis suggests factors =", fa_parallel$nfact, "\n\n")
} else {
  cat("Parallel analysis: hoppet over fordi det er for få svar eller for få variable items.\n")
  cat("\n")
}

## -------------------------
## 7. Faktoranalyse
## -------------------------

## 7a. Enfaktorløsning
fa_one <- NULL
fa_two <- NULL
if (nrow(analysis_matrix) >= analysis_min_rows && ncol(analysis_matrix) >= 3) {
  fa_one <- psych::fa(analysis_matrix, nfactors = 1, fm = "ml", rotate = "none")
  cat("Enfaktor-løsning (fa1):\n\n")
  print(fa_one$loadings)
  cat("\nSS loadings / Proportion Var:\n")
  print(fa_one$Vaccounted)
  cat("\n")

  fa_one_loadings <- as.data.frame(unclass(fa_one$loadings))
  write.csv(fa_one_loadings, "fa1_loadings.csv")

  ## 7b. Tofaktorløsning (for sammenligning)
  fa_two <- psych::fa(analysis_matrix, nfactors = 2, fm = "ml", rotate = "oblimin")
  cat("Tofaktor-løsning (fa2):\n\n")
  print(fa_two$loadings)
  cat("\nSS / Proportion / Cumulative Var:\n")
  print(fa_two$Vaccounted)
  cat("\n")

  fa_two_loadings <- as.data.frame(unclass(fa_two$loadings))
  write.csv(fa_two_loadings, "fa2_loadings.csv")
} else {
  cat("Faktoranalyse: hoppet over fordi det er for få svar eller for få variable items.\n")
  cat("\n")
}

## -------------------------
## 8. IRT – graded response model
## -------------------------
irt_model <- NULL
if (nrow(analysis_matrix) >= analysis_min_rows && ncol(analysis_matrix) >= 3) {
  cat("Fitter enfaktors IRT-modell (graded response)…\n\n")

  irt_model <- mirt(analysis_matrix, 1, itemtype = "graded")

  cat("IRT – faktorladninger og h2 (summary(mod_irt)):\n")
  print(summary(irt_model))
  cat("\n")

  cat("IRT – diskriminasjon (a) og terskler (b1–b4):\n")
  irt_coefficients <- coef(irt_model, IRTpars = TRUE)
  print(irt_coefficients)
  cat("\n")
} else {
  cat("IRT: hoppet over fordi det er for få svar eller for få variable items.\n")
  cat("\n")
}

## -------------------------
## 9. IRT-plott
## -------------------------
if (!is.null(irt_model)) {
  png("irt_test_info.png", width = 900, height = 600)
  plot(irt_model, type = "info")
  dev.off()

  png("irt_trace_plots.png", width = 1200, height = 900)
  plot(irt_model, type = "trace")
  dev.off()
}

## -------------------------
## 10. Oppsummering til konsollen
## -------------------------
cat("--------------------------------------------------\n")
cat("Antall observasjoner:", nrow(responses_matrix), "\n")
if (!is.null(alpha_out)) {
  cat("Cronbachs alfa (total):", round(alpha_out$total$raw_alpha, 3), "\n")
} else {
  cat("Cronbachs alfa: ikke beregnet.\n")
}
if (!is.null(fa_parallel)) {
  cat("Parallel analysis antydet faktortall:", fa_parallel$nfact, "\n")
} else {
  cat("Parallel analysis: ikke beregnet.\n")
}
if (!is.null(irt_model)) {
  cat("Enfaktor IRT-modell lagret i objektet 'irt_model'.\n")
  cat("Plott lagret til scree_plot.png, irt_test_info.png, irt_trace_plots.png\n")
} else {
  cat("IRT-modell: ikke beregnet.\n")
}
if (!is.null(fa_one) && !is.null(fa_two)) {
  cat("Lastinger skrevet til fa1_loadings.csv og fa2_loadings.csv\n")
} else {
  cat("Lastinger: ikke skrevet fordi faktoranalyse ble hoppet over.\n")
}
cat("--------------------------------------------------\n")
