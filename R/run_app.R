#' Run the LTMV Shiny Application
#'
#' @return An object representing the LTMV app
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}
