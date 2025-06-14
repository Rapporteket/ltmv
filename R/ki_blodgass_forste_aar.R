#' Kvalitetsindikator for oppfølging av blodgass første år.
#'
#' @description
#' Lag KI-datasett for andel forløp som fikk utført
#' blodgassmåling ved oppfølging - innen 2 år.
#' Dersom pasienten har avsluttet eller dødd innen
#' 2 år uten å ha besvart PROM, blir pasienten ikke
#' tatt med i beregningen av nevner eller teller.
#'
#' Tar inn et datasett på "superbredt"-format som
#' inneholder ventreg, ventfol (filtrert på year == 1)
#' og conclude.
#' Variabelnavnene i ventfol må i tillegg starte med
#' "f1_" (f.eks. "f1_qlwakeup").
#'
#' @param d_full_reg_forste_aar_ahoc Tar inn et datasett på
#' superbredt"-format som inneholder ventreg, ventfol
#' (filtrert for year == 1 og year == -1 (første adhoc))
#' og conclude (one-to-one).
#' Variabelnavnene i ventfol knyttet til year == 1 må i
#' tillegg starte med "f1_" (f.eks. "f1_qlwakeup") og
#' variabelnavnene i ventfol knyttet til year == -1 må
#' starte med "fah_" (f.eks. "fah_po2_air").
#' @param rapporteringsdato Tar inn en dato (År-Måned-Dag)
#' som er den siste datoen som blir tatt med i beregningen
#' (innen to år). F.eks. dersom datoen er "2023-12-31",
#' blir siste år med komplett data 2021.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export
ki_blodgass_forste_aar = function(d_full_reg_forste_aar_ahoc, rapporteringsdato) {
  d_full_reg_forste_aar_ahoc |>
    mutate(
      blodgass = !is.na(f1_pco2_air) |
        !is.na(f1_po2_air) |
        !is.na(f1_capillarypo2_air) |
        !is.na(f1_capillarypco2_air) |
        !is.na(f1_be) |
        !is.na(f1_arterialpco2_air) |
        !is.na(f1_transcutaneous_co2_air),
      diff_start_fah = difftime(fah_followup_date, dato_start, unit = "days"),
      fah_blodgass = diff_start_fah <= 730.5 & (!is.na(fah_pco2_air) |
        !is.na(fah_po2_air) |
        !is.na(fah_capillarypo2_air) |
        !is.na(fah_capillarypco2_air) |
        !is.na(fah_be) |
        !is.na(fah_arterialpco2_air) |
        !is.na(fah_transcutaneous_co2_air)),
      diff_start_stopp = case_when(
        !is.na(dato_stopp) & is.na(deceased_date) ~ dato_stopp - dato_start,
        is.na(dato_stopp) & !is.na(deceased_date) ~ deceased_date - dato_start,
        dato_stopp <= deceased_date ~ dato_stopp - dato_start,
        dato_stopp > deceased_date ~ deceased_date - dato_start,
        TRUE ~ NA
      ),
      ki_krit_nevner = case_when(
        ventilation_method == 3 ~ FALSE,
        year(dato_start) < 2002 ~ FALSE,
        blodgass ~ TRUE,
        fah_blodgass ~ TRUE,
        dato_start > !!rapporteringsdato - 730.5 ~ FALSE,
        diff_start_stopp <= 730.5 ~ FALSE,
        TRUE ~ TRUE
      ),
      ki_krit_teller = ki_krit_nevner & (blodgass | fah_blodgass)
    )
}
