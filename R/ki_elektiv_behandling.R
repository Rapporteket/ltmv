#' Kvalitetsindikator for elektiv behandling
#'
#' @description
#' Lag KI-datasett for andel forløp med elektiv (planlagt) behandlingsstart.
#'
#' @param d_ventreg Basisskjema.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export

ki_elektiv = function(d_ventreg) {
  d_ventreg |>
    mutate(
      ki_krit_teller = situation_ventilation == 2,
      ki_krit_nevner = !is.na(situation_ventilation) &
        (situation_ventilation == 1 |
          situation_ventilation == 2 |
          situation_ventilation == 9)
    )
}
