#' Common report processor for LTMV
#'
#' Makes reports for LTMV typically used for auto reports such as
#' subscriptions, dispatchments and bulletins. As such, please be warned that
#' any changes to this function might render existing auto reports nonfunctional
#' as they are based on static calls based on any previous version of this
#' function. Changes should therefore be thoroughly tested against existing auto
#' reports. Altering the names of the arguments will likely be a breaking
#' change. Adding new arguments should be safe as long as they are provided a
#' default value.
#'
#' @param report Character string identifying the report to be processed by this
#' function.
#' @param output_type Character string with output format. Must be one of
#' \code{c("html", "pdf")}. Defaults to "pdf".
#' @param title Character string giving the report title. Empty string by
#' default.
#' @param author Character string providing the report author. Default value is
#' "unknown author".
#' @param org_name Character string with the name of the organization/hospital.
#' Default is "unknown organization".
#' @param org_id Integer (?) with the id of the organization/hospital. Default is
#' 999999.
#' @param registry_name Character string with registry name. Default is
#' "norspis".
#' @param user_full_name Character string giving the person name, normally the
#' user requesting the report. Default is "unknown person name".
#' @param user_role Character string giving a user role, normally the one of the
#' user requesting the report. Default is "unknown role".
#'
#' @return A character string with a path to where the produced file is located.
#' @export
#'
#' @examples
#' ## Make the start page for LTMV
#' report_file_path <- report_processor(report = "guide",
#'                                      output_type = "html",
#'                                      title = "Example report")

report_processor <- function(report,
                             output_type = "pdf",
                             title = "",
                             author = "unknown author",
                             org_name = "unknown organization",
                             org_id = 999999,
                             registry_name = "ltmv",
                             user_full_name = "unknown person name",
                             user_role = "unknown role") {

  stopifnot(report %in% c("guide", "sample_report"))

  stopifnot(output_type %in% c("html", "pdf"))

  filePath <- NULL

  if (title == "") {
    warning("No title given! Reports should have a title...")
  }

  if (report == "guide") {
    filePath <- rapbase::renderRmd(
      system.file("guide.Rmd", package = "ltmv"),
      outputType = output_type,
      params = list(
        title = title,
        author = author,
        hospital_name = org_name,
        table_format = output_type,
        resh_id = org_id
      )
    )
  }

  if (report == "sample_report") {
    filePath <- rapbase::renderRmd(
      system.file("sample_report.Rmd", package = "ltmv"),
      outputType = output_type,
      params = list(
        title = title,
        author = author,
        hospital_name = org_name,
        table_format = output_type,
        resh_id = org_id
      )
    )
  }

  filePath
}
