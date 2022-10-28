#' Get data from LTMV
#'
#' Functions to query data from a database holding LTMV data at Rapporteket.
#' Providing these function with the shiny session object logging of queries
#' may also be performed
#'
#' @param registry_name Character string with name of the registry from which
#' data are to be queried
#' @param resh_id Integer providing the organization id. Useful for data
#' filtering
#' @param ... Optional arguments to pass to the function. If \code{session} is
#' provided this will be assumed a valid shiny session object and hence logging
#' may be performed
#'
#' @name query_data
#' @aliases query_all_hospitals
#' @return Data frame of registry data
NULL


#' @rdname query_data
#' @export
query_all_hospitals <- function(registry_name, resh_id, ...) {

  query <- paste0("
SELECT
  ID AS ReshId,
  CENTRENAME AS Navn,
  CENTRESHORTNAME AS Kortnavn
FROM
  centre;
  ")

  if ("session" %in% names(list(...))) {
    msg <- paste0("Query centre data from ", registry_name, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registry_name, query)
}
