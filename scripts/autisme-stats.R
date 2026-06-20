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
remote_db_path <- Sys.getenv("AUTISME_DB_REMOTE_PATH", "/srv/shiny-server/autisme-test/data/autisme.sqlite")
local_db_candidates <- c(
  Sys.getenv("AUTISME_DB_PATH", ""),
  file.path(getwd(), "autisme.sqlite"),
  file.path(getwd(), "data", "autisme.sqlite")
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
    paste0("  ", paste(item_columns, collapse = ",
  "), ","),
    "  MAX(language) AS language",
    "FROM", table_name,
    "GROUP BY timestamp",
    "ORDER BY timestamp;",
    sep = "
"
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
      host, ":", path, "
",
      paste(output, collapse = "
")
    )
  }

  text <- paste(output, collapse = "
")
  if (!nzchar(trimws(text))) {
    stop("Fikk ingen data fra SQLite-spørringen.")
  }

  read.csv(text = text, stringsAsFactors = FALSE)
}

read_sqlite_table <- function(table_name, item_ids) {
  sql <- build_wide_sql(table_name, item_ids)

  if (nzchar(local_db_path) && file.exists(local_db_path) && !dir.exists(local_db_path)) {
    cat("Leser lokal SQLite:
", normalizePath(local_db_path), "

", sep = "")
    read_local_sqlite(local_db_path, sql)
  } else {
    cat("Leser remote SQLite via SSH:
", remote_host, ":", remote_db_path, "

", sep = "")
    read_remote_sqlite(remote_host, remote_db_path, sql)
  }
}

response_item_ids <- paste0("item", 1:13)
responses_df <- read_sqlite_table("responses", response_item_ids)

cat("Leste ", nrow(responses_df), " rader.

", sep = "")

if ("timestamp" %in% names(responses_df)) {
  responses_df$timestamp <- as.POSIXct(responses_df$timestamp)
}
if ("language" %in% names(responses_df)) {
  responses_df$language <- trimws(responses_df$language)
}

responses_matrix <- as.data.frame(lapply(responses_df[, response_item_ids], as.numeric))

## -------------------------
## 3. Deskriptiv statistikk
## -------------------------
cat("Deskriptivstatistikk per item:
")
descriptive_items <- psych::describe(responses_matrix)
print(descriptive_items)
cat("
")

## gjennomsnittsskår per person
mean_score <- rowMeans(responses_matrix, na.rm = TRUE)
cat("Deskriptivstatistikk for gjennomsnittsskår:
")
print(psych::describe(as.data.frame(mean_score)))
cat("
")

## -------------------------
## 4. Korrelasjoner
## -------------------------
cat("Korrelasjonsmatrise:
")
correlation_matrix <- cor(responses_matrix, use = "pairwise.complete.obs")
print(round(correlation_matrix, 2))
cat("
")

## -------------------------
## 5. Intern konsistens
## -------------------------
alpha_out <- psych::alpha(responses_matrix)
cat("Cronbachs alfa:
")
print(alpha_out$total)
cat("
")

## -------------------------
## 6. Parallel analysis / faktortall
## -------------------------
cat("Parallel analysis (lagrer scree-plot til fil):
")

png("scree_plot.png", width = 800, height = 800)
fa_parallel <- psych::fa.parallel(
  responses_matrix,
  fm = "ml",
  fa = "fa",
  show.legend = TRUE,
  main = "Parallel Analysis Scree Plots"
)
dev.off()

cat("
Parallel analysis suggests factors =", fa_parallel$nfact, "

")

## -------------------------
## 7. Faktoranalyse
## -------------------------

## 7a. Enfaktorløsning
fa_one <- psych::fa(responses_matrix, nfactors = 1, fm = "ml", rotate = "none")
cat("Enfaktor-løsning (fa1):

")
print(fa_one$loadings)
cat("
SS loadings / Proportion Var:
")
print(fa_one$Vaccounted)
cat("
")

fa_one_loadings <- as.data.frame(unclass(fa_one$loadings))
write.csv(fa_one_loadings, "fa1_loadings.csv")

## 7b. Tofaktorløsning (for sammenligning)
fa_two <- psych::fa(responses_matrix, nfactors = 2, fm = "ml", rotate = "oblimin")
cat("Tofaktor-løsning (fa2):

")
print(fa_two$loadings)
cat("
SS / Proportion / Cumulative Var:
")
print(fa_two$Vaccounted)
cat("
")

fa_two_loadings <- as.data.frame(unclass(fa_two$loadings))
write.csv(fa_two_loadings, "fa2_loadings.csv")

## -------------------------
## 8. IRT – graded response model
## -------------------------
cat("Fitter enfaktors IRT-modell (graded response)…

")

irt_model <- mirt(responses_matrix, 1, itemtype = "graded")

cat("IRT – faktorladninger og h2 (summary(mod_irt)):
")
print(summary(irt_model))
cat("
")

cat("IRT – diskriminasjon (a) og terskler (b1–b4):
")
irt_coefficients <- coef(irt_model, IRTpars = TRUE)
print(irt_coefficients)
cat("
")

## -------------------------
## 9. IRT-plott
## -------------------------

png("irt_test_info.png", width = 900, height = 600)
plot(irt_model, type = "info")
dev.off()

png("irt_trace_plots.png", width = 1200, height = 900)
plot(irt_model, type = "trace")
dev.off()

## -------------------------
## 10. Oppsummering til konsollen
## -------------------------
cat("--------------------------------------------------
")
cat("Antall observasjoner:", nrow(responses_matrix), "
")
cat("Cronbachs alfa (total):", round(alpha_out$total$raw_alpha, 3), "
")
cat("Parallel analysis antydet faktortall:", fa_parallel$nfact, "
")
cat("Enfaktor IRT-modell lagret i objektet 'irt_model'.
")
cat("Lastinger skrevet til fa1_loadings.csv og fa2_loadings.csv
")
cat("Plott lagret til scree_plot.png, irt_test_info.png, irt_trace_plots.png
")
cat("--------------------------------------------------
")
