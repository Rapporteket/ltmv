#' Lag SPC-figur for blodgassindikator
#'
#' @description
#' Funksjonen tek inn ei dataramme med oppstartsskjema,
#' og lagar eit P-diagram med andel pasientar som har fått målt
#' blodgass per `tidseining`.
#'
#' @param d_ki_superbreitt
#' Dataramme på superbredt format som inneholder oppstartsskjema (ventreg)
#' med variabelnavn r_{variabel}, og oppfølingsskjema.
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
#' d_superbreitt = superbreitt_format(
#'   d_full_patientlist = hent_skjema("patient"),
#'   d_full_mce = hent_skjema("mce"),
#'   d_full_ventreg = hent_skjema("ventreg"),
#'   d_full_ventfol = hent_skjema("ventfol"),
#'   d_full_conclude = hent_skjema("conclude")
#' ) |>
#'   dplyr::filter(
#'     lubridate::year(start_date) >= format(Sys.Date() - years(6), "%Y")
#'   )
#'
#' lag_spcfigur_ki_blodgass(d_superbreitt)
#'
#' lag_spcfigur_ki_blodgass(d_superbreitt, tidseining = "year")
#' }
lag_spcfigur_ki_blodgass_forste_aar = function(d_ki_superbreitt, tidseining = "month") {
  d_ki_blodgass_forste_aar = d_ki_superbreitt |>
    ki_blodgass_forste_aar(dato_data = Sys.Date()) |>
    group_by(
      start_tid = lubridate::floor_date(r_start_date, unit = tidseining)
    ) |>
    rapwhale::aggreger_ki_prop()

  p = qicharts2::qic(
    x = start_tid,
    y = ki_teller,
    n = ki_nevner,
    data = d_ki_blodgass_forste_aar,
    chart = "p"
  )

  p + ggplot2::labs(caption = "tekst her") +
    ggplot2::theme(plot.title = ggplot2::element_blank()) +
    ggplot2::xlab("Startdato")
}
