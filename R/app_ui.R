#' Client (ui) for the ltmv app
#'
#' @return An shiny app ui object
#' @export

app_ui = function() {
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  app_title = "LTMV"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        app_title
      ),
      windowTitle = app_title,
      theme = "rap/bootstrap.css",
      id = "tabs",
      shiny::tabPanel(
        "Start",
        shiny::mainPanel(
          rapbase::renderRmd(
            system.file("guide.Rmd", package = "ltmv"),
            outputType = "html_fragment"
          ),
          rapbase::navbarWidgetInput("ltmv-navbar-widget")
        )
      ),
      shiny::tabPanel(
        "Eksempelrapport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::radioButtons("format_report",
              "Format for nedlasting",
              list(PDF = "pdf", HTML = "html"),
              inline = FALSE
            ),
            shiny::downloadButton("download_report", "Last ned!")
          ),
          shiny::mainPanel(
            shiny::htmlOutput("ex_report", inline = TRUE)
          )
        )
      ),
      shiny::tabPanel(
        "Alle sykehus",
        shiny::sidebarLayout(
          shiny::sidebarPanel(),
          shiny::mainPanel(
            shiny::htmlOutput("hospital_report", inline = TRUE)
          )
        )
      ),
      shiny::navbarMenu(
        "Figurer/tabeller",
        shiny::tabPanel(
          "Antall ferdige/uferdige skjema",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::dateRangeInput("dato_antall_skjema",
                label = "Dato:",
                start = "2014-01-01",
                end = lubridate::today(),
                min = "2014-01-01",
                max = lubridate::today(),
                language = "no",
                weekstart = 1,
                separator = " til "
              ),
              shiny::actionButton("alle_datoer_knapp", "Alt"),
              shiny::actionButton("innevarende_aar_knapp", "Inneværende år"),
              shiny::actionButton("tretti_dager_knapp", "Siste 30 dager"),
              shiny::actionButton("syv_dager_knapp", "Siste 7 dager"),
              shiny::checkboxGroupInput("alderkat",
                label = "Alder:",
                choices = c("Barn" = "barn", "Voksen" = "voksen", "Ukjent" = NA),
                selected = c("barn", "voksen", NA),
                inline = TRUE
              ),
              shiny::checkboxGroupInput("aktiv_behandling",
                label = "Aktiv behandling per i dag:",
                choices = c("Ja" = TRUE, "Nei" = FALSE),
                selected = c(TRUE, FALSE),
                inline = TRUE
              ),
              width = 3
            ),
            shiny::mainPanel(
              shiny::htmlOutput("antall_skjema")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Abonnement",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("ltmv-subscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("ltmv-subscription")
          )
        )
      ),
      shiny::navbarMenu(
        "Verkt\u00f8y",
        shiny::tabPanel(
          "Utsending",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::autoReportFormatInput("ltmv-dispatch-format"),
              rapbase::autoReportOrgInput("ltmv-dispatch-org"),
              rapbase::autoReportInput("ltmv-dispatch")
            ),
            shiny::mainPanel(
              rapbase::autoReportUI("ltmv-dispatch")
            )
          )
        ),
        shiny::tabPanel(
          "Bruksstatistikk",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::statsInput("ltmv-stats"),
              rapbase::statsGuideUI("ltmv-stats")
            ),
            shiny::mainPanel(
              rapbase::statsUI("ltmv-stats")
            )
          )
        ),
        shiny::tabPanel(
          "Eksport",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::exportUCInput("ltmv-export")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("ltmv-export")
            )
          )
        )
      )
    )
  )
}
