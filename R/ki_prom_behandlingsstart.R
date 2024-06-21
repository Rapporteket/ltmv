#' Kvalitetsindikator for PROM
#'
#' @description
#' Lag KI-datasett for andel pasienter som har besvart PROM ved behandlingsstart.
#'
#' @param d_prom PROM-datasett på langt format. Alle PROM-spørsmålene
#'     må være i én kolonne med tilhørende verdier i en annen.
#'     Datasettet må også inneholde en kolonne med navn "mangler_ikke"
#'     som har verdien 0 for spørsmål som ikke er besvart og verdien
#'     1 for spørsmål som er besvart.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export
#'
ki_prom = function(d_prom) {
  d_prom |>
    count(patient_id, mangler_ikke) |>
    arrange(patient_id, desc(mangler_ikke)) |>
    distinct(patient_id, .keep_all = TRUE) |>
    mutate(
      ki_krit_teller = mangler_ikke == 1,
      ki_krit_nevner = TRUE
    )
}
