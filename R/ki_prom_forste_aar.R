#' Kvalitetsindikator for PROM første år
#'
#' @description
#' Lag KI-datasett for andel pasienter som har besvart PROM
#' oppfølging - innen 2 år.
#' Dersom pasienten har avsluttet eller dødd innen 2 år uten
#' å ha besvart PROM, blir pasienten ikke tatt med i
#' beregningen av nevner eller teller.
#'
#' @param d_full_reg_forste_aar Tar inn et datasett på
#' "superbredt"-format som inneholder ventreg, ventfol
#' og conclude.
#' Variabelnavnene i ventfol må starte med "f1_" (f.eks. "f1_qlwakeup"),
#' variablene fra ventreg må starte med "r_" (f.eks. "r_dato_start") og
#' variablene fra conclude må starte med "c_" (f.eks. "c_dato_stopp").
#' @param rapporteringsdato Tar inn en dato (År-Måned-Dag)
#' som er den siste datoen som blir tatt med i beregningen
#' (innen to år). F.eks. dersom datoen er "2023-12-31",
#' blir siste år med komplett data 2021.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export
#' @examples
#' \dontrun{
# d_superbreitt = superbreitt_format(
#   d_full_patientlist = hent_skjema("patient"),
#   d_full_mce = hent_skjema("mce"),
#   d_full_ventreg = hent_skjema("ventreg"),
#   d_full_ventfol = hent_skjema("ventfol"),
#   d_full_conclude = hent_skjema("conclude")
# )
#'
#' ki_prom_forste_aar(d_superbreitt, dato_data = as.Date("2024-12-31"))
#' }
ki_prom_forste_aar = function(d_full_reg_forste_aar, rapporteringsdato) {
  d_full_reg_forste_aar |>
    mutate(
      gyldig_prom = !is.na(f1_qlwakeup) |
        !is.na(f1_qlheadache) |
        !is.na(f1_qlrested) |
        !is.na(f1_qlsleepy) |
        !is.na(f1_qldyspnoea) |
        !is.na(f1_qless),
      diff_start_stopp = case_when(
        !is.na(c_dato_stopp) & is.na(c_deceased_date) ~ c_dato_stopp - r_dato_start,
        is.na(c_dato_stopp) & !is.na(c_deceased_date) ~ c_deceased_date - r_dato_start,
        c_dato_stopp <= c_deceased_date ~ c_dato_stopp - r_dato_start,
        c_dato_stopp > c_deceased_date ~ c_deceased_date - r_dato_start,
        TRUE ~ NA
      ),
      ki_krit_nevner = case_when(
        gyldig_prom ~ TRUE,
        lubridate::year(r_dato_start) < 2014 ~ FALSE,
        r_dato_start > !!rapporteringsdato - 730.5 ~ FALSE,
        diff_start_stopp <= 730.5 ~ FALSE,
        TRUE ~ TRUE
      ),
      ki_krit_teller = ki_krit_nevner & gyldig_prom
    )
}
