#' Lag HTML-tabell med aktivitetsoversikt
#'
#' @description
#' Tek inn ei dataramme og lagar ein HTML-tabell med oversikt over
#' ulike nøkkeltal for aktivitet.
#'
#' @param d
#' Dataramme med minimum variablane `aktiv_behandling`, `start_date`,
#' `respcon`, `alder_no`, `gender` og `status`.
#'
#' @return
#' HTML-tabell med aktivitetsoversikt per dags dato i registeret.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d = hent_skjema("ventreg") |>
#'   legg_til_pasientid(mceid) |>
#'   legg_til_pasientinfo(patient_id) |>
#'   legg_til_stoppinfo(mceid) |>
#'   legg_til_aktiv_behandling() |>
#'   mutate(
#'     alder_no = lubridate::time_length(
#'       x = lubridate::interval(
#'         start = birth_date,
#'         end = Sys.Date()
#'       ),
#'       unit = "years"
#'     )
#'   )
#'
#' lag_aktivitetsoversikt(d)
#' }
lag_aktivitetsoversikt = function(d) {
  d_aktivitetsoversikt = d |>
    summarise(
      `i behandling nå` = sum(aktiv_behandling),
      `nye i år` = sum(
        lubridate::year(start_date) == lubridate::year(Sys.Date()),
        na.rm = TRUE
      ),
      `% med trakeostomi nå` = (sum(aktiv_behandling & respcon == 1,
        na.rm = TRUE
      ) / sum(aktiv_behandling)) * 100,
      `% under 18 år nå` = (sum(aktiv_behandling & alder_no < 18,
        na.rm = TRUE
      ) / sum(aktiv_behandling)) * 100,
      `% kvinner nå` = (sum(aktiv_behandling & gender == 2, na.rm = TRUE) /
        sum(aktiv_behandling)) * 100,
      `% ferdigstilte registreringsskjema (start)` =
        (sum(status == 1) / n()) * 100
    ) |>
    tidyr::pivot_longer(everything()) |>
    dplyr::relocate(value)

  d_aktivitetsoversikt |>
    knitr::kable(
      format = "html",
      col.names = NULL,
      digits = 0,
      format.args = list(big.mark = " ")
    ) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
}
