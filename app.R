library(shiny)
library(shinyjs)
library(tibble)
library(DBI)
library(RSQLite)
library(bslib)
library(shiny.i18n)
library(grendelshiny)
library(shinyseo)

options(bslib.cache = FALSE)

custom_theme <- bs_theme(
  version = 5,
  bg = "#031817",
  fg = "#EDFDFB",
  primary = "#76E9D9",
  secondary = "#B6FFF4",
  success = "#6FCF97",
  info = "#A7F3E9",
  warning = "#F2CF7F",
  danger = "#F28C8C",
  base_font = "Avenir Next",
  heading_font = "Avenir Next"
)

translations_csvs_path <- "./content/translations/"
i18n <- Translator$new(
  translation_csvs_path = translations_csvs_path,
  translation_csv_config = paste0(translations_csvs_path, "config.yml")
)
i18n$set_translation_language("nb")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    y
  } else {
    x
  }
}

map_browser_language <- function(browser_lang) {
  browser_lang <- tolower(browser_lang %||% "")

  if (startsWith(browser_lang, "nn")) return("nn")
  if (startsWith(browser_lang, "nb") || startsWith(browser_lang, "no")) return("nb")
  if (startsWith(browser_lang, "sv")) return("sv")
  if (startsWith(browser_lang, "da")) return("da")
  if (startsWith(browser_lang, "fi")) return("fi")
  if (startsWith(browser_lang, "smj") || startsWith(browser_lang, "smh")) return("nb")
  if (startsWith(browser_lang, "se")) return("se")
  if (startsWith(browser_lang, "fkv")) return("fkv")
  if (startsWith(browser_lang, "fr")) return("fr")
  if (startsWith(browser_lang, "es")) return("es")
  if (startsWith(browser_lang, "de")) return("de")
  if (startsWith(browser_lang, "pl")) return("pl")
  if (startsWith(browser_lang, "lt")) return("lt")
  if (startsWith(browser_lang, "uk")) return("uk")
  if (startsWith(browser_lang, "en")) return("en")

  "nb"
}

custom_css_href <- paste0(
  "custom.css?v=",
  format(file.info("./www/custom.css")$mtime, "%Y%m%d%H%M%S")
)

custom_js_href <- paste0(
  "custom.js?v=",
  format(file.info("./www/custom.js")$mtime, "%Y%m%d%H%M%S")
)

ensure_db_schema <- function(con) {
  dbExecute(con, "PRAGMA journal_mode = WAL;")
  dbExecute(con, "PRAGMA busy_timeout = 5000;")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS responses (timestamp TEXT NOT NULL, item_id TEXT NOT NULL, score REAL NOT NULL, language TEXT NOT NULL)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_timestamp ON responses(timestamp)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_item_id ON responses(item_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_language ON responses(language)")
}

core_items_r <- function() {
  tibble(
    id = paste0("item", 1:13),
    tekst = c(
      i18n$t("Jeg trenger sjelden å gjette hva folk egentlig mener når de snakker indirekte."),
      i18n$t("Jeg blir sjelden veldig urolig av brå endringer i planer."),
      i18n$t("Høye lyder, sterke lys eller andre sanseinntrykk overvelder meg sjelden."),
      i18n$t("Jeg klarer som regel sosiale regler og forventninger uten å måtte tenke mye over dem."),
      i18n$t("Jeg trenger sjelden å forberede eller øve på det jeg skal si i sosiale situasjoner."),
      i18n$t("Jeg oppfatter som regel ironi, humor og indirekte beskjeder uten stor innsats."),
      i18n$t("Sosialt samvær tømmer meg sjelden så mye at jeg trenger lang tid alene etterpå."),
      i18n$t("Jeg trenger sjelden faste rutiner for å føle meg trygg i hverdagen."),
      i18n$t("Jeg blir sjelden så opptatt av spesialinteresser at det går ut over andre ting jeg må gjøre."),
      i18n$t("Jeg opplever vanligvis blikkontakt, kroppsspråk og små sosiale signaler som ganske naturlige å håndtere."),
      i18n$t("Jeg føler sjelden behov for å maskere eller spille en rolle for å passe inn sosialt."),
      i18n$t("Små endringer i rekkefølge eller rutiner gjør meg sjelden tydelig urolig."),
      i18n$t("Jeg må sjelden trekke meg unna fordi sanseinntrykk blir for sterke.")
    )
  )
}

answer_score <- function(items) {
  stopifnot(length(items) == 13)
  as.numeric(mean(items))
}

ui <- fluidPage(
  title = i18n$t("Dette er ikke en autismetest"),
  social_meta("meta.yml"),
  grendelshiny::grendelshiny_css(),
  tags$link(rel = "stylesheet", type = "text/css", href = custom_css_href),
  theme = custom_theme,
  usei18n(i18n),
  useShinyjs(debug = FALSE),
  tags$head(
    grendelshiny::grendelshiny_js(),
    tags$script(src = custom_js_href),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update-title', function(msg) {
        document.title = msg;
      });
    "))
  ),
  div(
    class = "hero",
    div(
      class = "hero-copy",
      div(
        class = "hero-header",
        div(
          class = "hero-mark",
          tags$img(
            src = "grendel-g.webp",
            alt = "Grendel-logo",
            class = "hero-logo"
          )
        ),
        div(
          class = "hero-heading",
          span(class = "eyebrow", i18n$t("Refleksjon")),
          h1(i18n$t("Dette er ikke en autismetest"))
        )
      ),
      p(
        class = "hero-text",
        HTML(paste0(
          as.character(i18n$t("Denne siden viser hvordan fravær av sosial, sensorisk og rutinemessig friksjon kan se ut.")),
          " ",
          as.character(i18n$t("Den er ikke diagnostisk. Den kan verken bekrefte eller avkrefte autisme."))
        ))
      ),
    ),
    div(
      class = "hero-panel",
      h2(i18n$t("Kort om testen")),
      p(i18n$t("Denne siden viser hvordan fravær av sosial, sensorisk og rutinemessig friksjon kan se ut.")),
      p(i18n$t("Den er ikke diagnostisk. Den kan verken bekrefte eller avkrefte autisme."))
    )
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-card",
      selectizeInput(
        inputId = "selected_language",
        label = i18n$t("Skift språk"),
        choices = c("nb", "nn", "sv", "da", "fi", "se", "fkv", "fr", "es", "de", "pl", "lt", "uk", "en"),
        selected = "nb",
        options = list(
          render = I("
            {
              option: function(item, escape) {
                return Shiny.renderFlagOption(item);
              },
              item: function(item, escape) {
                return Shiny.renderFlagItem(item);
              }
            }
          ")
        )
      ),
      actionButton("beregn", i18n$t("Beregn resultat"))
    ),
    mainPanel(
      class = "main-card",
      tabsetPanel(
        id = "tabs",
        type = "pills",
        tabPanel(
          value = "spm",
          i18n$t("Spørsmål"),
          br(),
          p(i18n$t("Dra i hver slider for å velge hvor godt utsagnet har stemt for deg over tid.")),
          hr(),
          uiOutput("sporsmals_ui")
        ),
        tabPanel(
          value = "res",
          i18n$t("Resultat"),
          br(),
          h3(i18n$t("Tolkning")),
          uiOutput("score_meter"),
          br(),
          textOutput("resultat_tekst"),
          br(),
          h4(i18n$t("Hva betyr skåren?")),
          uiOutput("score_md"),
          h4(i18n$t("Forbehold")),
          p(i18n$t("En klinisk vurdering innebærer utviklingshistorie, funksjon og faglig skjønn.")),
          p(i18n$t("Mennesker kan ha sterke rutiner eller tydelig sansesensitivitet uten at det handler om autisme."))
        ),
        tabPanel(
          value = "om",
          i18n$t("Om testen"),
          br(),
          uiOutput("om_testen")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  detected_lang <- reactiveVal("nb")
  user_selected_lang <- reactiveVal(NULL)

  db_path <- file.path(
    Sys.getenv("AUTISME_DB_PATH", unset = "data"),
    Sys.getenv("AUTISME_DB_NAME", unset = "autisme.sqlite")
  )

  observeEvent(input$browser_lang, {
    initial_lang <- map_browser_language(input$browser_lang)
    detected_lang(initial_lang)

    if (is.null(user_selected_lang())) {
      updateSelectizeInput(session, "selected_language", selected = initial_lang)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$selected_language, {
    if (!is.null(input$selected_language) && nzchar(input$selected_language)) {
      user_selected_lang(input$selected_language)
    }
  }, ignoreInit = TRUE)

  lang <- reactive({
    user_selected_lang() %||% detected_lang()
  })

  observeEvent(lang(), {
    i18n$set_translation_language(lang())
    update_lang(language = lang(), session = session)
    shinyjs::html("beregn", i18n$t("Beregn resultat"))
    session$sendCustomMessage("update-title", i18n$t("Dette er ikke en autismetest"))
  }, ignoreInit = TRUE)

  validate_answers <- function() {
    ids <- core_items_r()$id
    svar <- lapply(ids, function(id) input[[id]])
    mangler <- vapply(svar, function(x) is.null(x) || is.na(x), logical(1))

    if (any(mangler)) {
      return(list(
        gyldig = FALSE,
        beskjed = i18n$t("Du må svare på alle utsagn før resultatet kan beregnes."),
        score = NA_real_,
        pct = NA_real_
      ))
    }

    svar_num <- as.numeric(unlist(svar))
    avg <- answer_score(svar_num)

    list(
      gyldig = TRUE,
      beskjed = NULL,
      score = avg,
      pct = (avg - 1) / 4 * 100
    )
  }

  output$sporsmals_ui <- renderUI({
    render_question <- function(item) {
      div(
        style = "width: 100%; max-width: 600px;",
        strong(item$tekst),
        div(
          class = "scale-labels",
          HTML(sprintf(
            "<span>%s</span>
 <span>%s</span>
 <span>%s</span>
 <span>%s</span>
 <span>%s</span>",
            i18n$t("Stemmer ikke"),
            i18n$t("Litt"),
            i18n$t("Delvis"),
            i18n$t("Ganske"),
            i18n$t("Stemmer helt")
          ))
        ),
        sliderInput(
          inputId = item$id,
          label = NULL,
          min = 1,
          max = 5,
          value = 3,
          step = 1,
          ticks = FALSE,
          width = "100%"
        ),
        br()
      )
    }

    items <- core_items_r()

    tagList(
      lapply(seq_len(nrow(items)), function(i) {
        render_question(items[i, ])
      })
    )
  })

  score_reaktiv <- eventReactive(input$beregn, {
    validate_answers()
  })

  observeEvent(input$beregn, {
    sc <- validate_answers()

    if (!sc$gyldig) {
      showNotification(sc$beskjed, type = "error")
      return()
    }

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
    current_language <- lang()

    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)

    ensure_db_schema(con)

    item_ids <- core_items_r()$id
    scores <- vapply(item_ids, function(id) input[[id]], numeric(1))

    df <- data.frame(
      timestamp = rep(timestamp, length(item_ids)),
      item_id = item_ids,
      score = scores,
      language = rep(current_language, length(item_ids))
    )

    dbWithTransaction(con, {
      dbWriteTable(con, "responses", df, append = TRUE)
    })

    shinyjs::disable("beregn")
    shinyjs::html("beregn", i18n$t("Sendt"))
    shinyjs::delay(3000, shinyjs::enable("beregn"))
    updateTabsetPanel(session, "tabs", selected = "res")
  }, ignoreInit = TRUE)

  output$score_md <- renderUI({
    current_lang <- lang()
    i18n$set_translation_language(current_lang)

    tagList(
      tags$p(i18n$t("Skåren er et enkelt gjennomsnitt av de tretten hovedutsagnene.")),
      tags$ul(
        tags$li(paste0("1 - ", i18n$t("Stemmer ikke"))),
        tags$li(paste0("5 - ", i18n$t("Stemmer helt")))
      ),
      tags$p(i18n$t("Høyere skår betyr at svarene dine ligner mindre på et mønster med sosial, sensorisk og rutinemessig friksjon som ofte beskrives ved autisme.")),
      tags$p(i18n$t("Lavere skår betyr at svarene dine ligner mer på et slikt mønster.")),
      tags$p(i18n$t("Dette er ikke en diagnose, og det finnes ingen fast norm for denne siden ennå. Bruk skåren som et utgangspunkt for refleksjon, ikke som et fasitsvar."))
    )
  })

  output$resultat_tekst <- renderText({
    sc <- score_reaktiv()

    if (is.null(sc) || is.na(sc$score)) {
      return(i18n$t("Svar på alle utsagn og trykk Beregn resultat for å se tolkningen."))
    }

    score_value <- sprintf("%.1f / 5", sc$score)
    interpretation <- if (sc$score < 2) {
      i18n$t("Svarene dine viser få trekk som ligner de autismerelevante friksjonene man ofte ser ved autisme.")
    } else if (sc$score < 2.75) {
      i18n$t("Hverdagen virker stabil og forutsigbar.")
    } else if (sc$score < 3.75) {
      i18n$t("Mønsteret ditt ligger godt innenfor normal variasjon: noen styrker, litt friksjon, men ingenting som peker klart i én retning.")
    } else if (sc$score < 4.5) {
      i18n$t("Du rapporterer en del trekk som kan minne om autisme, men dette kan like gjerne handle om personlighet, erfaringer, stress eller livssituasjon.")
    } else {
      i18n$t("Du beskriver flere områder som ofte skaper vansker ved autisme. Dette er fortsatt ikke diagnostikk, men det kan være verdt en mer formell vurdering dersom dette skaper problemer i hverdagen.")
    }

    paste0(i18n$t("Gjennomsnittsskår"), ": ", score_value, ". ", interpretation)
  })

  output$score_meter <- renderUI({
    sc <- score_reaktiv()

    if (is.null(sc) || is.na(sc$score)) {
      return(tags$p(i18n$t("Svar på alle utsagn og trykk Beregn resultat for å se gjennomsnittet.")))
    }

    score_value <- sprintf("%.1f / 5", sc$score)
    score_percent <- sprintf("%.0f%%", sc$pct)

    div(
      class = "score-meter",
      tags$div(class = "small text-muted mb-1", i18n$t("Gjennomsnittsskår")),
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$strong(score_value),
        span(score_percent)
      ),
      div(
        class = "progress",
        div(
          class = "progress-bar",
          role = "progressbar",
          style = sprintf("width: %.1f%%;", sc$pct),
          `aria-valuenow` = sprintf("%.1f", sc$score),
          `aria-valuemin` = "1",
          `aria-valuemax` = "5",
          score_value
        )
      )
    )
  })

  output$om_testen <- renderUI({
    current_lang <- lang()
    i18n$set_translation_language(current_lang)

    tagList(
      tags$p(i18n$t("Dette er en enkel refleksjonsside, ikke en diagnostisk test.")),
      tags$p(i18n$t("Den spør om trekk som ofte nevnes i forbindelse med autisme, særlig sosial tolkning, sansning, rutiner og maskering.")),
      tags$p(i18n$t("Den kan ikke bekrefte eller avkrefte autisme.")),
      tags$p(i18n$t("Den er laget for å vise hvordan et mønster med lite sosial, sensorisk og rutinemessig friksjon kan se ut.")),
      tags$p(i18n$t("Hvis du er usikker, eller hvis dette påvirker hverdagen, bør du snakke med fagfolk.")),
      tags$p(
        tags$a(
          href = "mailto:rolf@grendel.no?subject=Autisme-testen",
          "© 2026 Grendel AS"
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)

# Local Variables:
# mode: R
# End:
