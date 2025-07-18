#' Client (ui) for the ltmv app
#'
#' @return An shiny app ui object
#' @export

app_ui = function() {
  app_title = "LTMV"

  shiny::tagList(
    shiny::navbarPage(
      title = rapbase::title(app_title),
      windowTitle = app_title,
      theme = rapbase::theme(),
      id = "tabs",
      shiny::tabPanel(
        "Dashboard",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput("dato_dashboard",
              label = "Startdato:",
              start = "1961-01-01",
              end = lubridate::today(),
              min = "1961-01-01",
              max = lubridate::today(),
              language = "no",
              weekstart = 1,
              separator = " til "
            ),
            shiny::actionButton("vise_alt", "Alt"),
            shiny::actionButton("to_aar_knapp", "Siste 2 år"),
            shiny::actionButton("fem_aar_knapp", "Siste 5 år"),
            shiny::checkboxInput("inkluder_missing",
              label = "Inkluder manglende startdato",
              value = TRUE
            ),
            shiny::checkboxGroupInput("alderkat_dashboard",
              label = "Alder ved start:",
              choices = c(Barn = "barn", Voksen = "voksen", Ukjent = NA),
              selected = c("barn", "voksen", NA),
              inline = TRUE
            ),
            shiny::checkboxGroupInput("alderkat_naa_dashboard",
              label = "Alder nå:",
              choices = c(Barn = "barn", Voksen = "voksen", Ukjent = NA),
              selected = c("barn", "voksen", NA),
              inline = TRUE
            ),
            shiny::checkboxGroupInput("kjonn",
              label = "Kjønn:",
              choices = c(Mann = 1, Kvinne = 2, Ukjent = NA),
              selected = c(1, 2, NA),
              inline = FALSE
            ),
            shiny::uiOutput("enhet_dashboard"),
            shiny::uiOutput("rhf_dashboard"),
            shiny::uiOutput("hf_dashboard"),
            shiny::uiOutput("sykehus_dashboard"),
            width = 2
          ),
          shiny::mainPanel(
            shiny::fluidRow(
              shiny::column(
                width = 2,
                htmltools::tags$div(
                  "Aktivitetsoversikt",
                  style = "font-size: 22px;"
                ),
                shiny::tableOutput("aktivitetsoversikt")
              ),
              shiny::column(
                width = 6,
                shiny::plotOutput("diagnosefordeling")
              ),
              shiny::column(
                width = 4,
                shiny::plotOutput("aldersfordeling")
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                htmltools::tags$div(
                  "Blodgass målt før behandlingsstart",
                  style = "font-size: 22px;"
                ),
                shiny::plotOutput("blodgass_for")
              ),
              shiny::column(
                width = 4,
                "Blodgass målt første år",
                shiny::plotOutput("blodgass_forste_aar")
              )
            ),
            width = 10
          )
        ),
        rapbase::navbarWidgetInput("ltmv-navbar-widget", selectOrganization = TRUE)
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
              shiny::actionButton("eldre_enn_ett_aar", "> 1 år"),
              shiny::actionButton("innevarende_aar_knapp", "Inneværende år"),
              shiny::actionButton("tretti_dager_knapp", "Siste 30 dager"),
              shiny::actionButton("syv_dager_knapp", "Siste 7 dager"),
              shiny::checkboxGroupInput("alderkat",
                label = "Alder:",
                choices = c(Barn = "barn", Voksen = "voksen", Ukjent = NA),
                selected = c("barn", "voksen", NA),
                inline = TRUE
              ),
              shiny::checkboxGroupInput("aktiv_behandling",
                label = "Aktiv behandling per i dag:",
                choices = c(Ja = TRUE, Nei = FALSE),
                selected = c(TRUE, FALSE),
                inline = TRUE
              ),
              shiny::uiOutput("utvalgte_rhf"),
              shiny::uiOutput("rhf"),
              shiny::uiOutput("hf"),
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
