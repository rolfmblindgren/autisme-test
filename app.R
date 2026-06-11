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

  if (startsWith(browser_lang, "en")) {
    return("en")
  }

  "nb"
}

custom_css_href <- paste0(
  "custom.css?v=",
  format(file.info("./www/custom.css")$mtime, "%Y%m%d%H%M%S")
)

ensure_db_schema <- function(con) {
  dbExecute(con, "PRAGMA journal_mode = WAL;")
  dbExecute(con, "PRAGMA busy_timeout = 5000;")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS responses (timestamp TEXT NOT NULL, item_id TEXT NOT NULL, score REAL NOT NULL, language TEXT NOT NULL)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS experimental_responses (timestamp TEXT NOT NULL, item_id TEXT NOT NULL, score REAL NOT NULL, language TEXT NOT NULL)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_timestamp ON responses(timestamp)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_item_id ON responses(item_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_language ON responses(language)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experimental_responses_timestamp ON experimental_responses(timestamp)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experimental_responses_item_id ON experimental_responses(item_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experimental_responses_language ON experimental_responses(language)")
}

core_items_r <- function() {
  tibble(
    id = paste0("item", 1:10),
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
      i18n$t("Jeg opplever vanligvis blikkontakt, kroppsspråk og små sosiale signaler som ganske naturlige å håndtere.")
    )
  )
}

experimental_items_r <- function() {
  tibble(
    id = paste0("item", 11:13),
    tekst = c(
      i18n$t("Jeg føler sjelden behov for å maskere eller spille en rolle for å passe inn sosialt."),
      i18n$t("Små endringer i rekkefølge eller rutiner gjør meg sjelden tydelig urolig."),
      i18n$t("Jeg må sjelden trekke meg unna fordi sanseinntrykk blir for sterke.")
    )
  )
}

answer_score <- function(items) {
  stopifnot(length(items) == 10)
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
    tags$script(src = "custom.js"),
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
      div(
        class = "hero-badges",
        span(class = "hero-badge", i18n$t("Spørsmål")),
        span(class = "hero-badge", i18n$t("Resultat")),
        span(class = "hero-badge", i18n$t("Om dette"))
      )
    ),
    div(
      class = "hero-panel",
      h2(i18n$t("Kort om dette")),
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
        choices = c("nb", "en"),
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
          textOutput("resultat_tekst"),
          br(),
          h4(i18n$t("Gjennomsnittsskår")),
          uiOutput("score_meter"),
          br(),
          h4(i18n$t("Hva betyr skåren?")),
          uiOutput("score_md"),
          h4(i18n$t("Forbehold")),
          p(i18n$t("En klinisk vurdering innebærer utviklingshistorie, funksjon og faglig skjønn.")),
          p(i18n$t("Mennesker kan ha sterke rutiner eller tydelig sansesensitivitet uten at det handler om autisme."))
        ),
        tabPanel(
          value = "om",
          i18n$t("Om dette"),
          br(),
          htmlOutput("om_testen")
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

    core_items <- core_items_r()
    exploratory_items <- experimental_items_r()

    tagList(
      lapply(seq_len(nrow(core_items)), function(i) {
        render_question(core_items[i, ])
      }),
      tags$hr(style = "margin: 1.5rem 0;"),
      div(
        style = "width: 100%; max-width: 600px; margin-bottom: 0.75rem; font-style: italic; color: #555;",
        i18n$t("Foreløpige spørsmål om sosial, sensorisk og rutinemessig belastning. Disse tre spørsmålene brukes ikke i hovedskåren ennå.")
      ),
      lapply(seq_len(nrow(exploratory_items)), function(i) {
        render_question(exploratory_items[i, ])
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

    core_item_ids <- core_items_r()$id
    core_scores <- vapply(core_item_ids, function(id) input[[id]], numeric(1))

    core_df <- data.frame(
      timestamp = rep(timestamp, length(core_item_ids)),
      item_id = core_item_ids,
      score = core_scores,
      language = rep(current_language, length(core_item_ids))
    )

    experimental_item_ids <- experimental_items_r()$id
    experimental_scores <- vapply(experimental_item_ids, function(id) input[[id]], numeric(1))

    experimental_df <- data.frame(
      timestamp = rep(timestamp, length(experimental_item_ids)),
      item_id = experimental_item_ids,
      score = experimental_scores,
      language = rep(current_language, length(experimental_item_ids))
    )

    dbWithTransaction(con, {
      dbWriteTable(con, "responses", core_df, append = TRUE)
      dbWriteTable(con, "experimental_responses", experimental_df, append = TRUE)
    })

    shinyjs::disable("beregn")
    shinyjs::html("beregn", i18n$t("Sendt"))
    shinyjs::delay(3000, shinyjs::enable("beregn"))
    updateTabsetPanel(session, "tabs", selected = "res")
  }, ignoreInit = TRUE)

  output$score_md <- renderUI({
    current_lang <- lang()
    file <- file.path("content", paste0(current_lang, ".score.md"))

    if (!file.exists(file)) {
      file <- file.path("content", "nb.score.md")
    }

    includeMarkdown(file)
  })

  output$resultat_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) {
      return("")
    }

    s <- res$score

    if (s >= 4.25) {
      paste0(
        i18n$t("Svarene dine viser få trekk som ligner de autismerelevante friksjonene man ofte ser ved autisme."),
        " ",
        i18n$t("Hverdagen virker stabil og forutsigbar.")
      )
    } else if (s >= 3.5) {
      i18n$t("Mønsteret ditt ligger godt innenfor normal variasjon: noen styrker, litt friksjon, men ingenting som peker klart i én retning.")
    } else if (s >= 2.75) {
      i18n$t("Du rapporterer en del trekk som kan minne om autisme, men dette kan like gjerne handle om personlighet, erfaringer, stress eller livssituasjon.")
    } else {
      i18n$t("Du beskriver flere områder som ofte skaper vansker ved autisme. Dette er fortsatt ikke diagnostikk, men det kan være verdt en mer formell vurdering dersom dette skaper problemer i hverdagen.")
    }
  })

  output$score_meter <- renderUI({
    sc <- score_reaktiv()
    if (!sc$gyldig) {
      return(NULL)
    }

    avg <- as.numeric(sc[["score"]])
    pct <- as.numeric(sc[["pct"]])

    bar_col <- if (avg < 2.75) {
      "#d9534f"
    } else if (avg < 3.5) {
      "#f0ad4e"
    } else {
      "#5cb85c"
    }

    tagList(
      div(
        style = "font-size: 32px; font-weight: 700; margin: 6px 0;",
        paste0(i18n$t("Gjennomsnittsskår"), " = ", format(round(avg, 1), nsmall = 1), " / 5")
      ),
      div(
        style = "height:14px; background:#eee; border-radius:999px; overflow:hidden;",
        div(style = paste0(
          "height:100%; width:", pct, "%; background:", bar_col, ";"
        ))
      ),
      div(
        style = "position:relative; height:16px; margin-top:6px; font-size:12px; color:#444;",
        span("1", style = "position:absolute; left:0%; transform:translateX(-50%);") ,
        span("2", style = "position:absolute; left:25%; transform:translateX(-50%);") ,
        span("3", style = "position:absolute; left:50%; transform:translateX(-50%);") ,
        span("4", style = "position:absolute; left:75%; transform:translateX(-50%);") ,
        span("5", style = "position:absolute; left:100%; transform:translateX(-50%);")
      )
    )
  })

  output$om_testen <- renderText({
    lg <- i18n$get_translation_language()
    res <- switch(
      lg,
      nb = "Dette er en enkel refleksjonsside, ikke en diagnostisk test. Den spør om trekk som ofte nevnes i forbindelse med autisme, særlig sosial tolkning, sansning, rutiner og maskering. Den kan ikke bekrefte eller avkrefte autisme. Den er laget for å vise hvordan et mønster med lite sosial, sensorisk og rutinemessig friksjon kan se ut. Hvis du er usikker, eller hvis dette påvirker hverdagen, bør du snakke med fagfolk.",
      en = "This is a simple reflection page, not a diagnostic test. It asks about traits that are often mentioned in connection with autism, especially social interpretation, sensory processing, routines, and masking. It cannot confirm or rule out autism. It is meant to show what a pattern with little social, sensory, and routine-related friction can look like. If you are unsure, or if this affects daily life, you should talk with a qualified professional.",
      "Dette er en enkel refleksjonsside, ikke en diagnostisk test. Den kan ikke bekrefte eller avkrefte autisme."
    )

    paste0("<p>", res, "</p><p><a href='mailto:rolf@grendel.no?subject=Autisme-testen'>© 2026 Grendel AS</a></p>")
  })
}

shinyApp(ui = ui, server = server)

# Local Variables:
# mode: R
# End:
