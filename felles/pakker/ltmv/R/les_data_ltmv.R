# Grunnmappe for datafiler (pakkeintern variabel)
grunnmappe_ltmv = "***FJERNA-ADRESSE***"

#' Les inn LTMV-data
#'
#' @description
#' Les inn LTMV-datadumpar og lagra dei som objekt, eventuelt med automatisk validering.
#'
#' @param mappe_dd Grunnmappe som datadumpar skal hentast frå. Må ha undermapper
#'     med datonamn (format: `ÅÅÅÅ-MM-DD`). Viss `NULL` (standard), vert standardmappa
#'     med PROD-data brukt.
#' @param dato Datoen for den aktuelle datadumpen (anten tekst på formatet `ÅÅÅÅ-MM-DD`
#'     eller eit [base::Date]-objekt). Vert brukt til å velja rett undermappe
#'     til `mappe_dd`. Viss `NULL` (standard), vert nyaste dato brukt.
#' @param maksdato Dato for siste prosedyre som skal vera med i datauttrekket (same datoformat
#'     som for `dato`). Oppføringar som tilhøyrer seinare prosedyredatoar,
#'     vert filtrerte ut, både frå prosedyreskjemaet og andre skjema.
#'     Viss `NULL` (standard), vert det ikkje noko filtrering på prosedyredato.
#' @param status Type oppføringar som skal lesast. Sjå [rapwhale::les_dd_oqr()]
#'     for informasjon om moglege verdiar. Som standard vert berre ferdigstilte oppføringar lesne.
#' @param valider Skal dataa validerast, dvs. sjekkast for feilverdiar før innlesing? (Standard: ja.)
#'     Omfattar både testar ved bruk av kodeboka og registerspesifikke testar.
#' @param omgjevnad Omgjevnad som dataobjekta skal lagrast til. Standard er den globale omgjevnaden.
#'
#' @details
#' Alle standard datafiler til registeret vert lesne inn, eventuelt validerte,
#' filtrerte (sjå nedanfor) og så lagra som R-objekt. Dei får namna `d_skjemanamn`, der `skjemanamn`
#' er namnet på det aktuelle skjemaet (for eksempel `d_ventfol` for oppfølgingskjemaet).
#' Som standard vert dei lagra i den globale omgjevnaden, og dei vil overskriva eventuelle
#' eksisterande objekt med same namn.
#'
#' I tillegg til alle standardskjemaa som er nemnde i kodeboka, vert datasett for
#' `mce`- og `patientlist`-filene òg lagra.
#'
#' Kodeboka for registeret vert òg lagra, med namnet `kb`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Les inn ferdigstilte skjema (nyaste datadumpar)
#' les_data_ltmv()
#' }
les_data_ltmv = function(mappe_dd = NULL, dato = NULL, maksdato = NULL,
                         status = 1, valider = TRUE, omgjevnad = .GlobalEnv) {
  if (is.null(mappe_dd)) {
    mappe_dd = grunnmappe_ltmv
  }

  # ID brukt i datadump-filnamn
  register_id = "Nasjonalt_register_for_langtids_mekanisk_ventilasjon"

  # lagre dato for datadump
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = lubridate::as_date(dato)
  assign("datadump_dato", dato, envir = omgjevnad)

  # Standard kodebok
  kb = rapwhale::les_kb_oqr(mappe_dd, reg_id = register_id, dato = dato, valider_kb = valider)
  assign("kb", kb, envir = omgjevnad)

  # Må legge til variabelen "year" i kodeboken. I følge dokumentasjonen
  # finnes den kun i databasen og vises ikke i registerskjemaet (ventfol).
  # Den dukker derfor ikke opp i kodeboken.
  ekstra_var = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id, ~variabeletikett, ~variabeltype, ~unik, ~obligatorisk, ~desimalar,
    "ventfol", "Avslutt", "year", "Antall år etter registrering", "numerisk", "nei", "ja", 0L
  )
  kb = bind_rows(kb, ekstra_var)

  # Heimesnikra kodebøker for registerspesifikke filer
  # Fixme: Bør oppdaterast når me får ny datadumpinnlesar for OQR
  #        (og kanskje endrast til å bruka les_csv_oqr()?)
  # Fixme: Må dokumenterast kor me har fått informasjon om kva
  #        variablar og variabeltypar kvar fil har (til no har
  #        me berre gjetta på bakgrunn av filene?).
  #        Må meldast JIRA-saker der det manglar dokumentasjon.
  kb_mce = tibble::tibble(
    skjema_id = "mce",
    variabel_id = tolower(c(
      "MCEID", "CENTREID", "PATIENT_ID",
      "MCETYPE", "HISTORICAL", "PARENT_MCE",
      "END_DATE", "MCE_TOTAL_STATUS",
      "REGISTRATION_DATE", "LAST_FOLLOWUP_YEAR",
      "STATUS", "CREATEDBY", "UPDATEDBY",
      "FIRST_TIME_CLOSED", "FIRST_TIME_CLOSED_BY",
      "TSCREATED", "TSUPDATED"
    )),
    variabeltype = c(
      "numerisk", "tekst", "numerisk", # CENTREID kan vera "TEST001"
      "numerisk", "numerisk", "numerisk",
      "dato", "numerisk",
      "dato", "numerisk",
      "numerisk", "tekst", "tekst",
      "dato_kl", "dato_kl", # fixme: må sjekke variabeltypene
      "dato_kl", "dato_kl"
    ),
    verdi = NA_character_, verditekst = NA_character_,
    desimalar = NA_integer_, min = NA_real_, maks = NA_real_,
    obligatorisk = c(
      TRUE, TRUE, TRUE, # fixme: må sjekke hvilke variabler som er obligatoriske
      TRUE, FALSE, FALSE,
      FALSE, FALSE,
      FALSE, FALSE,
      TRUE, TRUE, FALSE,
      FALSE, FALSE,
      TRUE, FALSE
    )
  )
  kb_pas = tibble::tibble(
    skjema_id = "patientlist",
    variabel_id = tolower(c(
      "PasientID", "RegistreringsDato",
      "Fodselsdato", "Kjonn", "Avdod",
      "Dodsdato", "Postnummer",
      "Poststed", "Kommune", "Fylke"
    )),
    variabeltype = c(
      "numerisk", "dato",
      "dato", "numerisk", "numerisk",
      "dato", "numerisk",
      "tekst", "tekst", "tekst"
    ),
    verdi = NA_character_, verditekst = NA_character_,
    desimalar = NA_integer_, min = NA_real_, maks = NA_real_,
    obligatorisk = c(
      TRUE, TRUE, TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    )
  ) # fixme: må sjekke om disse er obligatoriske
  les_og_lagra = function(skjema, status, kb) {
    d = rapwhale::les_dd_oqr(mappe_dd,
      reg_id = register_id, skjema_id = skjema,
      status = status, dato = dato, kodebok = kb,
      valider_kb = TRUE, valider_dd = valider
    )
    if (skjema == "ventreg" && !is.null(maksdato)) { # Andre skjema vert *indirekte* filtrerte på start_date (dato for behandlingsstart)
      d = dplyr::filter(d, start_date <= !!maksdato)
    }
    objektnamn = paste0("d_full_", skjema)
    assign(objektnamn, d, envir = omgjevnad)
  }

  # Les inn fullstendige datasett (utan filtrering på forløp)
  kb_skjema = setdiff(kb$skjema_id, "patient") # Manglar datafil for pasienttabellen
  purrr::walk(kb_skjema, les_og_lagra, status = status, kb = kb)
  les_og_lagra("mce", status = status, kb = kb_mce)
  les_og_lagra("patientlist", status = NULL, kb = kb_pas) # Datafila mangler STATUS-kolonne, så inga filtrering på dette
}
