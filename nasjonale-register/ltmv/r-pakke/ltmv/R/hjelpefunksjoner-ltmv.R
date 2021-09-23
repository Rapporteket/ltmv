# Hjelpefunksjoner som brukes i LTMV

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join case_when group_by summarise
#' @importFrom readxl read_excel
NULL

#' Legg til overordnede diagnosegrupper
#'
#' @param d_ventreg
#'
#' @return
#' @export
#'
legg_til_overordnet_diag = function(d_ventreg) {
  grunnmappe = "***FJERNA-ADRESSE***"

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
#'
#' @return
#' @export
#'
legg_til_hf_rhf_navn = function(d) {
  grunnmappe = "***FJERNA-ADRESSE***"

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
    select(hf_resh, hf_tekst, hf_gr, hf_gr_tekst)

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
#'
#' @return
#' @export
#'
legg_til_oppdaterte_fylker_og_rekkefolge_helseregion = function(d) {

  # Legger til navn på fylker, omkoding til nye fylkesnummer,
  # og kode for rekkefølgen som skal brukes for helseregioner.
  # Henter inn data på fylkeskoder og helseregion tilhørighet.
  d_fylke_til_rhf = read_excel("H:\\kvalreg\\fellesting\\dokumentasjon\\kodeboker\\fylke-rhf.xlsx")
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
#' @param trakeostomi_type
#'
#' @return
#' @export
#'
regn_ut_gj_trakestomi = function(d, trakeostomi_type) {
  round(100 * mean((d %>% filter(!is.na(respcon)))$respcon == trakeostomi_type))
}


#' Regn ut antall og prosent for diagnosegrupper
#'
#' @param d
#' @param alderkat
#'
#' @return
#' @export
#'
regn_antall = function(d, alderkat = TRUE) {
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
#' @param alderkat
#' @param type
#' @param str_orden
#'
#' @return
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
#' @param ...
#'
#' @return
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
#' @param var
#'
#' @return
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
