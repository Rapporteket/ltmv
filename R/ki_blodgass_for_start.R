#' Kvalitetsindikator for blodgass
#'
#' @description
#' Lag KI-datasett for andel forløp som fikk utført blodgassmåling ved start.
#'
#' @param d_ventreg Basisskjema.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export

ki_blodgass = function(d_ventreg) {
  d_ventreg %>%
    mutate(
      ki_krit_teller = (!is.na(pco2_air) |
                          !is.na(po2_air) |
                          !is.na(capillarypo2_air) |
                          !is.na(capillarypco2_air) |
                          !is.na(be) |
                          !is.na(arterialpco2_air)),
      ki_krit_nevner = TRUE
    )
}
