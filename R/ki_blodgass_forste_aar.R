#' Kvalitetsindikator for oppfølging av blodgass første år.
#'
#' @description
#' Lag KI-datasett for andel forløp som fikk utført
#' blodgassmåling ved oppfølging - innen 2 år.
#' Dersom pasienten har avsluttet eller dødd innen
#' 2 år uten å fått utført blodgassmåling ved
#' oppfølging, blir pasienten ikke tatt med i
#' beregningen av nevner eller teller.
#' Pasienter som er behandlet med CPAP skal ikke
#' inkluderes i beregningen. Tilsvarende for
#' pasienter som startet behandling før 2002
#' (registeret ble elektronisk, og ble nasjonalt).
#'
#' Tar inn et datasett på "superbredt"-format som
#' inneholder ventreg, ventfol (filtrert på year == 1)
#' og conclude.
#' Variabelnavnene i ventfol må i tillegg starte med
#' "f1_" (f.eks. "f1_qlwakeup").
#'
#' @param d_superbreitt Tar inn et datasett på
#' superbredt"-format som inneholder ventreg, ventfol
#' (filtrert for year == 1 og year == -1 (første adhoc))
#' og conclude (one-to-one).
#' Variabelnavnene i ventfol knyttet til year == 1 må i
#' tillegg starte med "f1_" (f.eks. "f1_qlwakeup") og
#' variabelnavnene i ventfol knyttet til year == -1 må
#' starte med "fah_" (f.eks. "fah_po2_air").
#' @param dato_data Tar inn en dato (År-Måned-Dag)
#' som er den siste datoen som blir tatt med i beregningen
#' for når oppfølgingen må ha vært gjennomført (innen to år).
#' F.eks. dersom datoen er "2023-12-31", blir siste år
#' med komplett data 2021.
#'
#' @return KI-datasett egnet for bruk med [rapwhale::aggreger_ki_prop()].
#'
#' @export
ki_blodgass_forste_aar = function(d_superbreitt, dato_data) {
    mutate(
      blodgass = !is.na(f1_pco2_air) |
        !is.na(f1_po2_air) |
        !is.na(f1_capillarypo2_air) |
        !is.na(f1_capillarypco2_air) |
        !is.na(f1_be) |
        !is.na(f1_arterialpco2_air) |
        !is.na(f1_transcutaneous_co2_air),
      diff_start_fah = difftime(fah_followup_date, r_start_date, unit = "days"),
      fah_blodgass = diff_start_fah <= 730.5 & (!is.na(fah_pco2_air) |
        !is.na(fah_po2_air) |
        !is.na(fah_capillarypo2_air) |
        !is.na(fah_capillarypco2_air) |
        !is.na(fah_be) |
        !is.na(fah_arterialpco2_air) |
        !is.na(fah_transcutaneous_co2_air)),
      diff_start_stopp = case_when(
        !is.na(c_stop_date) & is.na(c_deceased_date) ~ c_stop_date - r_start_date,
        is.na(c_stop_date) & !is.na(c_deceased_date) ~ c_deceased_date - r_start_date,
        c_stop_date <= c_deceased_date ~ c_stop_date - r_start_date,
        c_stop_date > c_deceased_date ~ c_deceased_date - r_start_date,
        TRUE ~ NA
      ),
      ki_krit_nevner = case_when(
        r_ventilation_method == 3 ~ FALSE,
        year(r_start_date) < 2002 ~ FALSE,
        blodgass ~ TRUE,
        fah_blodgass ~ TRUE,
        r_start_date > !!rapporteringsdato - 730.5 ~ FALSE,
        diff_start_stopp <= 730.5 ~ FALSE,
        TRUE ~ TRUE
      ),
      ki_krit_teller = ki_krit_nevner & (blodgass | fah_blodgass)
    )
}
