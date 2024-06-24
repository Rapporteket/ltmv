#' Funksjon for tilgjengelighet av behandling (insidens og prevalens)
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
#' \dontrun{
#' ins_akt_rhf = regn_insprev(d_pers_folketall_akt,
#'   folketall_rhf,
#'   insidens = TRUE,
#'   rhf_kode, rhf_tekst
#' )
#' }
regn_insprev = function(d, var_folk, insidens = TRUE, ...) {
  var_gruppe = rlang::enquos(...)
  var_folk = rlang::enquo(var_folk)

  d = d |>
    group_by(!!!var_gruppe)

  if (insidens) {
    d = d |>
      reframe(n_akt = n(), insidens = 100000 * (n_akt / !!var_folk))
  } else {
    d = d |>
      reframe(n_reg = n(), prevalens = 100000 * (n_reg / !!var_folk))
  }

  d |>
    distinct(!!!var_gruppe, .keep_all = TRUE)
}
