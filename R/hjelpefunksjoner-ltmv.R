# Hjelpefunksjoner som brukes i LTMV

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join case_when group_by summarise
#' @importFrom readxl read_excel
NULL

#' Legg til overordnede diagnosegrupper
#'
#' @param d_ventreg
#' Tibble/dataramme med data fra skjemaet ventreg,
#' f.eks. hentet med [les_data_ltmv()].
#' @param grunnmappe
#' Tekststreng med adresse til ltmv sin kvalitetsserver.
#'
#' @return
#' Inndatarammen `d_ventreg` med ekstra kolonner med info om overordnede
#' diagnosegrupper lagt til.
#' @export
#'
legg_til_overordnet_diag = function(d_ventreg, grunnmappe) {
  # Datadumpen til LTMV inneholder finkornede diagnoser,
  # men i alle figurer samler vi en eller flere i overordnede grupper.
  # Diagnosegrupper er kodet i en egen fil på kvalitetsserveren.
  mappe_kb = paste0(grunnmappe, "kodebok\\")
  filnavn_kb_diag = "diagnose-grupper.xlsx"
  kb_diag = read_excel(paste0(mappe_kb, filnavn_kb_diag))

  # Gjør om "diagnose" til en tekstvariabel for at det skal samsvare med registeret
  kb_diag$diagnose = as.character(kb_diag$diagnose)

  # Kobler diagnosegruppene til oppstartsskjemaet (ventreg)
  # Merk at DIAGNOSIS1 er bare hoveddiagnosen.
  # DIAGNOSIS2 inneholder bidiagnosen, og har samme diagnosekoder,
  # kobler man til disse vil man få diagnosegruppene til disse også
  # (diagnoser burde vært i en tabell på langt format)
  # fixme! I rapporten benytter vi oss bare av pasientens hoveddiagnose (DIAGNOSIS1),
  # men det hadde vært mer riktig i opptelling av antall, insidens etc. å se på
  # bidiagnosen (DIAGNOSIS2) også.
  d_ventreg = d_ventreg %>%
    left_join(kb_diag, by = c("diagnosis1" = "diagnose"))

  d_ventreg
}


#' Legg til HF-navn og RHF-navn
#'
#' @param d
#' Tibble/dataramme med data fra et av skjemaene ventreg,
#' ventfol eller conclude,
#' f.eks. hentet med [les_data_ltmv()].
#' @param grunnmappe
#' Tekststreng med adresse til ltmv sin kvalitetsserver.
#'
#' @return
#' Inndatarammen `d` med ekstra kolonner med info om HF lagt til.
#' @export
#'
legg_til_hf_rhf_navn = function(d, grunnmappe) {
  mappe_kb = paste0(grunnmappe, "kodebok\\")

  # Tar inn sykehusdata for å få navn på foretak
  # henter ut foretaksdata og tar bort "HF" navn på foretak.
  # Helseregionnavn får vi fra fylkeskobling lenger nede.
  filnavn_kb_sjukehus = "s_registerresh_sjukehusinfo.xlsx"
  d_sjukehus_info = read_excel(paste0(mappe_kb, filnavn_kb_sjukehus)) %>%
    mutate(
      hf_tekst = str_replace_all(hf_tekst, "\\ HF", ""),
      hf_gr_tekst = str_replace_all(hf_gr_tekst, "\\ HF", "")
    )

  hf_akt = d_sjukehus_info %>%
    distinct(hf_resh, .keep_all = TRUE) %>%
    select(hf_resh, hf_tekst, hf_gr, hf_gr_tekst) # FIXME Funksjonen legg berre til info om HF, ikkje RHF som funksjonsnamnet seier?

  # Gjør om "hf_resh" til en tekstvariabel for at det skal samsvare med registeret
  hf_akt$hf_resh = as.character(hf_akt$hf_resh)

  # Legger til foretaksnavnene + helseregionsnavnene
  d = d %>%
    left_join(hf_akt, by = c("move_to_centre" = "hf_resh")) %>%
    rename(hf_resh = move_to_centre)

  d
}


#' Legg til oppdaterte fylkesnavn og rekkefølge på helseregioner
#'
#' @param d
#' Tibble/dataramme med data fra et av skjemaene ventreg,
#' ventfol eller conclude,
#' f.eks. hentet med [les_data_ltmv()].
#'
#' @return
#' Inndatarammen `d` med ekstra kolonner med info om fylke og RHF lagt til.
#'
#' @export
#'
legg_til_oppdaterte_fylker_og_rekkefolge_helseregion = function(d) {
  # FIXME Anna namn på funksjonen, den legg ikkje til rekkefølge, men RHF-namn og resh? (I tillegg til info om fylke)

  # Legger til navn på fylker, omkoding til nye fylkesnummer,
  # og kode for rekkefølgen som skal brukes for helseregioner.
  # Henter inn data på fylkeskoder og helseregion tilhørighet.
  fylke_rhf_adresse = system.file("extdata", "fylke-rhf.xlsx", package = "ltmv")
  d_fylke_til_rhf = read_excel(fylke_rhf_adresse)
  d_fylker = d_fylke_til_rhf %>%
    select(starts_with("fylker"))

  # Gjør om "fylker_historisk" til en tekstvariabel for at det skal samsvare med registeret
  d_fylker$fylker_historisk = as.character(d_fylker$fylker_historisk)

  d = d %>%
    left_join(d_fylker, by = c("kode_fylke" = "fylker_historisk"))

  # Vi vil også ha navn på helseregioner.
  # Registeret har en variabel som koder for rekkefølgen
  # som brukes i grafer, fra nord til sør.
  d_rhf = d_fylke_til_rhf %>%
    select(starts_with("rhf")) %>%
    distinct(rhf_kode, .keep_all = TRUE)

  # Gjør om "hf_resh" til en tekstvariabel for at det skal samsvare med registeret
  d_rhf$rhf_kode = as.character(d_rhf$rhf_kode)

  d = d %>%
    rename(rhf_kode = move_to_rhf) %>%
    left_join(d_rhf, by = "rhf_kode")

  d
}


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
    d_n_diag = d %>%
      count(alderkat, diag_gruppe, diag_gruppe_navn) %>%
      group_by(alderkat)
  } else {
    d_n_diag = d %>%
      count(diag_gruppe, diag_gruppe_navn)
  }
  # regner ut prosent
  d_n_diag = d_n_diag %>%
    mutate(pro = (n / sum(n)))

  # sette rekkefølgen basert på størst til minst.
  rekkefolge_diag = d %>%
    arrange(diag_gruppe) %>%
    distinct(diag_gruppe) %>%
    pull("diag_gruppe")
  rekkefolge_diag_navn = d %>%
    arrange(diag_gruppe) %>%
    distinct(diag_gruppe_navn) %>%
    pull("diag_gruppe_navn")
  d_n_diag = d_n_diag %>%
    mutate(diagnose = factor(diag_gruppe,
      levels = rev(rekkefolge_diag),
      labels = rev(rekkefolge_diag_navn)
    ))

  # spytter ut resultat
  d_n_diag
}


#' Hent ut antall, navn, eller prosent for diagnosegrupper av ulik størrelse
#'
#' @param d_n_diaggruppe_akt
#' Tibble/dataramme med diagnosegrupper, alderskategori, antall og andel.
#' @param alderkat
#' Tekstvektor med alderskategorien skal det sjekkes for, "Barn" eler "Voksen".
#' @param type
#' Tekstvektor med navnet på kolonnen som utverdien skal hentes fra.
#' @param str_orden
#' Heltall som sier hvilken diagnosegruppe det skal hentes tall for.
#' Henter for den n-te største diagnosegruppen etter antall.
#'
#' @return
#' Verdien i kolonnen `type` for den `str_orden`-te største diagnosegruppen
#' etter antall i `d_n_diaggruppe_akt`,
#' filtrert på alderskategori `alderkat`.
#'
#' @export
#'
finn_storrelse_diag = function(d_n_diaggruppe_akt, alderkat, type, str_orden) {
  # gjør disse klare for bangbang
  alderkat = quo_name(enquo(alderkat))
  type = enquo(type)

  # filtrerer ut aktuell aldergruppe, og henter ut ønsket nivå (største, minste osv.)
  # av ønsket kolonne
  resultat = d_n_diaggruppe_akt %>%
    filter(alderkat == !!alderkat) %>%
    arrange(desc(n)) %>%
    pull(!!type) %>%
    nth(str_orden)

  resultat
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
  d_n_pas = d %>%
    distinct(patient_id, .keep_all = TRUE) %>%
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
  d = d %>%
    summarise(
      teller = sum(!is.na(!!!var)),
      nevner = n(), .groups = "drop"
    ) %>%
    mutate(prop = teller / nevner)
  d
}
