# Grunnmappe for datafiler (pakkeintern variabel)
grunnmappe_ablanor = "***FJERNA-ADRESSE***"

#' Les inn AblaNor-data
#'
#' @description
#' Les inn AblaNor-datadumpar og lagra dei som objekt, eventuelt med automatisk validering.
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
#' er namnet på det aktuelle skjemaet (for eksempel `d_basereg` for basisskjemaet).
#' Som standard vert dei lagra i den globale omgjevnaden, og dei vil overskriva eventuelle
#' eksisterande objekt med same namn.
#'
#' I AblaNor skal berre skjema som høyrer til forløp som har resultert i ein prosedyre
#' (eventuelt ein avbroten ein) analyserast. I objekta nemnde ovanfor er derfor oppføringar
#' for andre forløp filtrerte vekk. Viss ein person for eksempel berre har eit basisskjema
#' men ikkje (enno) eit prosedyreskjema, vil personen også vera filtrert vekk frå
#' basisskjema-datsettet (og forløpsdatasettet, pasientdatasettet og andre datasett).
#'
#' Men dei *ufiltrerte* datasetta vert også lagra, då med namn på forma `d_full_skjemanamn`,
#' for eksempel `d_full_basereg` for basisdatasettet. Merk at desse «fullversjonane»
#' vanlegvis *ikkje* skal brukast til analysar. Men dei kan vera nyttige når ein skal
#' sjå på ferdigstillings&#173;statistikk (gjerne i kombinasjon med `status =  NULL`
#' og eventuelt `maksdato = NULL`). Merk at også alle fullversjonane er filtrerte på `status`,
#' men det er berre prosedyreskjemaet som er filtrert på `maksdato` (prosedyredato).
#' Kort sagt er forskjellen mellom `d_*`- og `d_full_*`-objekta at førstnemnde
#' berre inneheld skjema der det finst eit tilhøyrande prosedyreskjema.
#'
#' I tillegg til alle standardskjemaa som er nemnde i kodeboka, vert datasett for
#' `mce`- og `patientlist`-filene òg lagra. Den filtrete versjonen av `patientlist`
#' inneheld berre pasientar som har minst eitt forløp i prosedyreskjemaet.
#' Kodeboka for registeret vert òg lagra, med namnet `kb`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Les inn ferdigstilte skjema (nyaste datadumpar)
#' les_data_ablanor()
#' }
les_data_ablanor = function(mappe_dd = NULL, dato = NULL, maksdato = NULL,
                            status = 1, valider = TRUE, omgjevnad = .GlobalEnv) {
  if (is.null(mappe_dd)) {
    mappe_dd = grunnmappe_ablanor
  }

  # ID brukt i datadump-filnamn
  register_id = "AblaNor"

  # Standard kodebok
  kb = rapwhale::les_kb_oqr(mappe_dd, reg_id = register_id, dato = dato, valider_kb = valider)
  assign("kb", kb, envir = omgjevnad)

  # Heimesnikra kodebøker for registerspesifikke filer
  # Fixme: Bør oppdaterast når me får ny datadumpinnlesar for OQR
  #        (og kanskje endrast til å bruka les_csv_oqr()?)
  kb_mce = tibble::tibble(
    skjema_id = "mce",
    variabel_id = tolower(c(
      "MCEID", "CENTREID",
      "MCETYPE", "PATIENT_ID", "STATUS", "MAIL_STATUS",
      "TSCREATED", "CREATEDBY", "TSUPDATED",
      "UPDATEDBY"
    )),
    variabeltype = c(
      "numerisk", "tekst", # CENTREID kan vera "TEST001"
      "numerisk", "numerisk", "numerisk", "numerisk",
      "dato_kl", "tekst", "dato_kl", "dato_kl"
    ),
    verdi = NA_character_
  )
  kb_pas = tibble::tibble(
    skjema_id = "patientlist",
    variabel_id = tolower(c(
      "ID", "REGISTERED_DATE", "BIRTH_DATE",
      "GENDER", "DECEASED", "DECEASED_DATE",
      "MUNICIPALITY_NUMBER", "MUNICIPALITY_NAME", "COUNTY"
    )),
    variabeltype = c(
      "numerisk", "dato", "dato",
      "numerisk", "numerisk", "dato",
      "numerisk", "numerisk", "numerisk"
    ),
    verdi = NA_character_
  )

  les_og_lagra = function(skjema, status, kb) {
    d = rapwhale::les_dd_oqr(mappe_dd,
      reg_id = register_id, skjema_id = skjema,
      status = status, dato = dato, kodebok = kb,
      valider_kb = FALSE, valider_dd = valider
    )
    if (skjema == "pros" && !is.null(maksdato)) { # Andre skjema vert *indirekte* filtrerte på prosedyredato
      d = dplyr::filter(d, dato_pros <= !!maksdato)
    }
    objektnamn = paste0("d_full_", skjema)
    assign(objektnamn, d, envir = omgjevnad)
  }

  # fixme: Legg til validering

  # Les inn fullstendige datasett (utan filtrering på forløp)
  kb_skjema = setdiff(kb$skjema_id, "patient") # Manglar datafil for pasienttabellen
  purrr::walk(kb_skjema, les_og_lagra, status = status, kb = kb)
  les_og_lagra("mce", status = status, kb = kb_mce)
  les_og_lagra("patientlist", status = NULL, kb = kb_pas) # Datafila mangler STATUS-kolonne, så inga filtrering på dette

  # I AblaNor skal me berre sjå på forløp som har resultert
  # i prosedyrar (inkl. avbrotne prosedyrar). Filtrerer derfor
  # ut eventuelle andre forløp.
  mceid_akt = omgjevnad$d_full_pros$mceid
  filtrer_og_lagra = function(skjema) {
    objektnamn_full = paste0("d_full_", skjema)
    objektnamn_filtrert = paste0("d_", skjema)
    d_full = get(objektnamn_full, envir = omgjevnad)
    d_filtrert = dplyr::filter(d_full, mceid %in% !!mceid_akt)
    assign(objektnamn_filtrert, d_filtrert, envir = omgjevnad)
  }
  purrr::walk(kb_skjema, filtrer_og_lagra)
  filtrer_og_lagra("mce")
  # Pasientlista har (naturleg nok) ikkje mceid, så der ser me berre
  # på pasientane som òg har eit aktuelt *forløp* (dvs. eit forløp som
  # finst i «d_mce», altså implisitt ein prosedyre som finst i «d_pros»).
  assign("d_patientlist",
    filter(
      get("d_full_patientlist", envir = omgjevnad),
      id %in% get("d_mce", envir = omgjevnad)$patient_id
    ),
    envir = omgjevnad
  )
}
