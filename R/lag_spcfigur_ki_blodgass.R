#' Lag SPC-figur for blodgassindikator
#'
#' @description
#' Funksjonen tek inn ei dataramme med oppstartsskjema,
#' og lagar eit P-diagram med andel pasientar som har fått målt
#' blodgass per `tidseining`.
#'
#' @param d_ventreg
#' Dataramme med oppstartsskjema.
#' @param tidseining
#' Tekstring med tidseining for SPC-diagrammet som vert laga.
#' Sjå argumentet `unit` i [lubridate::floor_date()] for detaljar om
#' moglege verdiar.
#' Standardverdi er `"month"`.
#'
#' @return
#' P-diagram med andel pasientar som har fått målt blodgass per `tidseining`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d_ventreg = hent_skjema("ventreg") |>
#'   dplyr::filter(lubridate::year(start_date) >= 2019)
#'
#' lag_spcfigur_ki_blodgass(d_ventreg)
#'
#' lag_spcfigur_ki_blodgass(d_ventreg, tidseining = "year")
#' }
lag_spcfigur_ki_blodgass = function(d_ventreg, tidseining = "month") {
  d_ki_blodgass = d_ventreg |>
    ki_blodgass() |>
    group_by(
      start_tid = lubridate::floor_date(start_date, unit = tidseining)
    ) |>
    rapwhale::aggreger_ki_prop()

  qicharts2::qic(
    x = start_tid,
    y = ki_teller,
    n = ki_nevner,
    data = d_ki_blodgass,
    chart = "p"
  ) +
    ggplot2::theme(plot.title = ggplot2::element_blank()) +
    ggplot2::xlab("Startdato")
}
