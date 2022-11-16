#' Client (ui) for the ltmv app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  app_title <- "LTMV"

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
                                inline = FALSE),
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
