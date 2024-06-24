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
#' (filtrert på year == 1) og conclude (one-to-one).
#' Variabelnavnene i ventfol må i tillegg starte med "f1_"
#' (f.eks. "f1_qlwakeup").
#' @param rapporteringsdato Tar inn en dato (År-Måned-Dag)
#' som er den siste datoen som blir tatt med i beregningen
#' (innen to år). F.eks. dersom datoen er "2023-12-31",
#' blir siste år med komplett data 2021.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export
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
        !is.na(dato_stopp) & is.na(deceased_date) ~ dato_stopp - dato_start,
        is.na(dato_stopp) & !is.na(deceased_date) ~ deceased_date - dato_start,
        dato_stopp <= deceased_date ~ dato_stopp - dato_start,
        dato_stopp > deceased_date ~ deceased_date - dato_start,
        TRUE ~ NA
      ),
      ki_krit_nevner = case_when(
        gyldig_prom ~ TRUE,
        year(dato_start) < 2014 ~ FALSE,
        dato_start > !!rapporteringsdato - 730.5 ~ FALSE,
        diff_start_stopp <= 730.5 ~ FALSE,
        TRUE ~ TRUE
      ),
      ki_krit_teller = ki_krit_nevner & gyldig_prom
    )
}
