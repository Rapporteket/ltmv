#' Server logic for the ltmv app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting ltmv application")

  registry_name <- "ltmv"
  hospital_name <- "Udefinert avdeling/sykehus"
  user_full_name <- rapbase::getUserFullName(session)
  user_role <- rapbase::getUserRole(session)
  user_resh_id <- rapbase::getUserReshId(session)

  rapbase::navbarWidgetServer("ltmv-navbar-widget", "ltmv", caller = "ltmv")

  # sample report
  output$ex_report <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("sample_report.Rmd", package = "ltmv"),
      outputType = "html_fragment",
      params = list(
        author = user_full_name,
        hospital_name = hospital_name,
        table_format = "html",
        resh_id = user_resh_id,
        registry_name = registry_name,
        user_role = user_role
      )
    )
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "ltmv-sample_report",
                        fileext = paste0(".", input$format_report)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("sample_report.Rmd", package = "ltmv"),
        outputType = input$format_report,
        params = list(
          author = user_full_name,
          hospital_name = hospital_name,
          table_format = input$format_report,
          resh_id = user_resh_id,
          registry_name = registry_name,
          user_full_name = user_full_name,
          user_role = user_role
        )
      )
      file.rename(fn, file)
    }
  )

  # simple table report
  output$hospital_report <- shiny::renderTable({
    query_all_hospitals(registry_name, user_resh_id, session = session)
  })



  # dummy report and orgs to subscribe and dispatch
  orgs <- list(
    TestOrg = 999999
  )
  report <- list(
    Veiledning = list(
      synopsis = "Testrapport kun for illustrasjon",
      fun = "report_processor",
      paramNames = c("report", "output_format", "title"),
      paramValues = c("guide", "pdf", "Testrapport")
    ),
    Eksempelrapport = list(
      synopsis = "Eksempelrapport med data fra LTMV",
      fun = "report_processor",
      paramNames = c("report", "output_format", "title"),
      paramValues = c("sample_report", "pdf", "Eksempelrapport")
    )
  )

  # subscribe
  rapbase::autoReportServer(
    "ltmv-subscription", registryName = registry_name, type = "subscription",
    reports = report, orgs = orgs
  )

  # dispatch
  org <- rapbase::autoReportOrgServer("ltmv-dispatch-org", orgs)
  file_format <- rapbase::autoReportFormatServer("ltmv-dispatch-format")
  param_names <- shiny::reactive(c("output_format"))
  param_values <- shiny::reactive(c(file_format()))
  rapbase::autoReportServer(
    "ltmv-dispatch", registryName = registry_name, type = "dispatchment",
    org = org$value,
    paramNames = param_names, paramValues = param_values, reports = report,
    orgs = orgs
  )

  # use stats
  rapbase::statsServer("ltmv-stats", registryName = registry_name)

  # export
  rapbase::exportGuideServer("ltmv-export", registry_name)
  rapbase::exportUCServer("ltmv-export", registry_name)
}
