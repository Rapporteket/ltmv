#' Regn ut prosent for andel trakeostomi eller maske (for bruk i teksten)
#'
#' @param d
#' Tibble/dataramme med data fra et av skjemaene ventreg eller ventfol,
#' f.eks. hentet med [les_data_ltmv()].
#' @param trakeostomi_type
#' Hvilken type ventilering skal det regnes ut andel for?
#' Verdien 1 for trakeostomi, verdien 2 for non-invasiv.
#'
#' @return
#' Andel forløp i `d` med ventilering av typen `trakeostomi_type`.
#' (I prosent, med én desimal og uten prosenttegn, altså andel * 100)
#' @export
#'
regn_ut_gj_trakestomi = function(d, trakeostomi_type) {
  round(100 * mean( # FIXME Gje ut som andel, kan gjerast om til prosent seinare med prosent()
    (filter(d, !is.na(respcon)))$respcon == trakeostomi_type
  ), digits = 1)
}

#' Regn ut antall og prosent for diagnosegrupper
#'
#' @param d
#' Tibble/dataramme med data fra skjemae ventreg,
#' f.eks. hentet med [les_data_ltmv()],
#' med info om diagnosegruppe lagt til med [legg_til_overordnet_diag()],
#' og en kolonne `alderkat` som sier om pasienten er barn (under 18 år)
#' eller voksen (18 år eller eldre).
#' @param alderkat
#' Boolsk variabel som sier om det skal gupperes på kolonnen `alderkat` i `d`.
#' Standardverdi er `TRUE`.
#'
#' @return
#' Tibble med oppsumert antall og andel pasienter de ulike diagnosegruppene,
#' eventuelt gruppert på alderskategori (barn/voksen) også.
#'
#' @export
#'
regn_antall = function(d, alderkat = TRUE) {
  # FIXME alderkat er namnet på både boolsk argument og kolonne i datasettet
  # vi grupperer bare på alderkat hvis ønskelig
  if (alderkat) {
    d_n_diag = d |>
      count(alderkat, diag_gruppe, diag_gruppe_navn) |>
      group_by(alderkat)
  } else {
    d_n_diag = d |>
      count(diag_gruppe, diag_gruppe_navn)
  }
  # regner ut prosent
  d_n_diag = d_n_diag |>
    mutate(pro = (n / sum(n)))

  # sette rekkefølgen basert på størst til minst.
  rekkefolge_diag = d |>
    arrange(diag_gruppe) |>
    distinct(diag_gruppe) |>
    pull("diag_gruppe")
  rekkefolge_diag_navn = d |>
    arrange(diag_gruppe) |>
    distinct(diag_gruppe_navn) |>
    pull("diag_gruppe_navn")
  d_n_diag = d_n_diag |>
    mutate(diagnose = factor(diag_gruppe,
      levels = rev(rekkefolge_diag),
      labels = rev(rekkefolge_diag_navn)
    ))

  # spytter ut resultat
  d_n_diag
}

#' Regn ut antall pasienter gitt en gruppe
#'
#' @param d
#' Tibble/dataramme.
#' @param ...
#' Kolonner det skal grupperes på.
#'
#' @return
#' Tibble med kolonnene gitt til funksjonen i `...`,
#' samt kolonnen `n` som gir antallet rader i `d` med alle kombinasjoner av
#' verdiene i kolonnene gitt i `...`.
#'
#' @export
#'
regn_n_pas = function(d, ...) {
  var_gruppe = enquos(...) # variabler som er gruppene man ønsker regne antall på
  d_n_pas = d |>
    distinct(patient_id, .keep_all = TRUE) |>
    count(!!!var_gruppe)
  d_n_pas
}


#' Finn andel som har en måling for sentrale variabler brukt i årsrapporten
#'
#' @param d
#' Tibble/dataramme.
#' @param var
#' Kolonne det skal regnes antall og andel missing for.
#'
#' @return
#' Tibble/dataramme med 3 kolonner og 1 rad.
#' Kolonnene er `teller` (antall rader i `d` som har en verdi i kolonnen `var`),
#' `nevner` (antall rader i `d`) og `prop`
#' (andel rader i `d` som har en verdi i kolonnen `var`(`teller/nevner`)).
#'
#' @export
#'
regn_andel_missing = function(d, var) {
  var = enquos(var)
  d = d |>
    summarise(
      teller = sum(!is.na(!!!var)),
      nevner = n(), .groups = "drop"
    ) |>
    mutate(prop = teller / nevner)
  d
}
