#' Run the LTMV Shiny Application
#'
#' @return An object representing the LTMV app
#' @export

run_app = function() {
  shiny::shinyApp(ui = ltmv::app_ui, server = ltmv::app_server)
}
