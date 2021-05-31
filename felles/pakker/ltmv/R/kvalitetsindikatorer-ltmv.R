# Kvalitetsindikatorer for LTMV

#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise distinct
NULL

#' Kvalitetsindikator for tilgjengelighet av behandling (insidens og prevalens)
#'
#' @description
#' Regner ut insidens og prevalens.
#'
#' @details
#' Krever et datasett som enten er filtrert på rapporteringsaar (til insidens)
#' eller som inneholder data fra hele registeret / alle år (for prevalens).
#'
#' @param d Datasett som enten er filtrert på rapporteringsaar
#' (til insidens) eller som inneholder data fra hele registeret /
#' alle år (for prevalens).
#' @param var_folk Variabel i `d` som inneholder folketall.
#' @param insidens Skal det regnes ut insidens eller prevalens?
#' Standard er insidens = TRUE.
#' @param ... Grupperingsvariabler
#'
#' @return
#' Returnerer en tibble med insidens eller prevalens, samt antall
#' registreringer, per gruppe.
#'
#' @export
#'
#' @examples
#' # d_pers_folketall_akt er et datasett som inneholder LTMV-pasienter
#' # som er registrert i rapporteringsåret.
#' ins_akt_rhf = regn_insprev(d_pers_folketall_akt,
#'   folketall_rhf,
#'   insidens = TRUE,
#'   rhf_kode, rhf_tekst
#' )
regn_insprev = function(d, var_folk, insidens = TRUE, ...) {
  var_gruppe = enquos(...)
  var_folk = enquo(var_folk)

  d = d %>%
    group_by(!!!var_gruppe)

  if (insidens) {
    d = d %>%
      summarise(n_akt = n(), insidens = 100000 * (n_akt / !!var_folk), .groups = "drop")
  } else {
    d = d %>%
      summarise(n_reg = n(), prevalens = 100000 * (n_reg / !!var_folk), .groups = "drop")
  }

  d %>%
    distinct(!!!var_gruppe, .keep_all = TRUE)
}

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
  d_ventreg %>%
    mutate(
      ki_krit_teller = situation_ventilation == 2,
      ki_krit_nevner = situation_ventilation == 1 |
        situation_ventilation == 2 |
        situation_ventilation == 9
    )
}

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
  d_prom %>%
    count(patient_id, mangler_ikke) %>%
    arrange(patient_id, desc(mangler_ikke)) %>%
    distinct(patient_id, .keep_all = TRUE) %>%
    mutate(
      ki_krit_teller = mangler_ikke == 1,
      ki_krit_nevner = !is.na(patient_id)
    )
}
